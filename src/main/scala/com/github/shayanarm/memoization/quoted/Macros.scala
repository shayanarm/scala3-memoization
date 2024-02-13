package com.github.shayanarm.memoization.quoted

import scala.reflect.Manifest
import scala.collection.mutable
import scala.quoted.*
import scala.PolyFunction
import scala.annotation.experimental
import scala.util.Success
import com.github.shayanarm.memoization.Storage

object Macros:
  private def resolveStorage[S](using
      Quotes,
      Type[S]
  ): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    val supported = Map(
      TypeRepr.of[scala.collection.mutable.WeakHashMap.type] -> TypeRepr
        .of[Storage.WeakHashMap],
      TypeRepr.of[scala.collection.mutable.HashMap.type] -> TypeRepr
        .of[Storage.HashMap]
    )
    supported
      .find((k, v) => k =:= TypeRepr.of[S])
      .fold(
        throw Exception(
          s"""
          |Unsupported storage for memoization: ${TypeRepr
              .of[S]
              .show(using Printer.TypeReprAnsiCode)}.
          |Supported storages are: 
          |${supported.keySet
              .map(t => s"- ${t.show(using Printer.TypeReprAnsiCode)}")
              .mkString("\n")}
          """.stripMargin
        )
      )(_._2)

  @experimental
  def memoizeImpl[S](using
      Quotes,
      Type[S]
  )(expr: Expr[Any]): Expr[Any] =
    import quotes.reflect.*
    memoize(expr, resolveStorage[S])

  @experimental
  private def memoize(using
      q: Quotes
  )(expr: Expr[Any], storage: q.reflect.TypeRepr): Expr[Any] =
    import quotes.reflect.*

    val widened = expr.asTerm.tpe.widen.asType match
      case '[w] => '{ ${ expr.asExprOf[w] }: w }
    val result = widened.asTerm.tpe match
      // This is a good old monomorphic function with no shenanigans
      case f @ AppliedType(
            TypeRef(_, name),
            types
          ) if f.isFunctionType =>
        val (args, rt) = types.splitAt(types.size - 1)
        memoizeMonomorphic(widened.asTerm, storage)(args, rt.head)
      // This is a monomorphic function with dependent types
      case f @ Refinement(
            tr,
            "apply",
            mt: MethodType
          ) if tr.isFunctionType =>
        memoizeRefinement(widened.asTerm, storage)(tr, mt)
      // This is a polymorphic function
      case Refinement(
            tr,
            "apply",
            pt: PolyType
          ) if tr =:= TypeRepr.of[PolyFunction] =>
        memoizeRefinement(widened.asTerm, storage)(tr, pt)
      case tpe if tpe.isFunctionType =>
        throw MatchError(
          widened.asTerm.tpe.show(using Printer.TypeReprStructure)
        )
      case _ => expr
    widened.asTerm.tpe.asType match
      // A final seal to guarantee the memoized function
      // has the exact same signature as the original one
      case '[t] =>
        '{ ${ result.asExprOf[t] }: t }

  @experimental
  private def memoizeMonomorphic(using
      Quotes
  )(original: quotes.reflect.Term, storageType: quotes.reflect.TypeRepr)(
      args: List[quotes.reflect.TypeRepr],
      rt: quotes.reflect.TypeRepr
  ): Expr[Any] =
    import quotes.reflect.*

    // We cannot create the context functions using the reflect API. We can however,
    // Do so using quotations ('{...}), so we will cover some arities of context functions to a
    // reasonable extent and a little beyond
    if original.tpe.isContextFunctionType then
      return memoizeContextFunction(original, storageType)(
        args,
        rt
      )

    val closureType = original.tpe.widen

    val parents =
      List(
        TypeTree.of[Object],
        TypeTree.of(using closureType.asType),
        TypeTree.of(using storageType.asType)
      )

    val cls = Symbol.newClass(
      Symbol.spliceOwner,
      "$anon",
      parents = parents.map(_.tpe),
      cls =>
        List(
          Symbol.newMethod(
            cls,
            "apply",
            MethodType((1 to args.size).map(i => s"x$i").toList)(
              _ => args,
              _ => rt
            ),
            Flags.Override,
            Symbol.noSymbol
          )
        ),
      selfType = None
    )
    val applySym = cls.declaredMethod("apply").head
    val applyDef = DefDef(
      applySym,
      argss => {
        given q2: Quotes = applySym.asQuotes
        val storage = This(cls).asExprOf[Storage]
        val body = rt.asType match
          case '[o] =>
            getOrEvalExpr[o](
              q2.reflect.TypeRepr.of(using storageType.asType),
              storage,
              argss.flatten.map(_.asExpr),
              argss
                .foldLeft[Term](
                  Select
                    .unique(original, "apply")
                ) { (c, i) => c.appliedToArgs(i.asInstanceOf[List[Term]]) }
                .asExpr
            )
        Some(body.asTerm)
      }
    )
    val clsDef = ClassDef(cls, parents, body = List(applyDef))
    val closure =
      Block(
        List(clsDef),
        Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil)
      )
    closure.asExpr

  @experimental
  private def memoizeContextFunction(using
      Quotes
  )(original: quotes.reflect.Term, storageType: quotes.reflect.TypeRepr)(
      args: List[quotes.reflect.TypeRepr],
      rt: quotes.reflect.TypeRepr
  ): Expr[Any] =
    import quotes.reflect.*
    def withStorage[A](using Type[A])(f: Expr[Storage => A]): Expr[A] =
      '{
        val storage: Storage = ${
          storageType.asType match
            case '[Storage.WeakHashMap] => '{ new Storage.WeakHashMap {} }
            case '[Storage.HashMap]     => '{ new Storage.HashMap {} }
        };
        ${ Expr.betaReduce('{ $f(storage) }) }
      }

    (args.map(_.asType), rt.asType) match
      case (List('[t1]), '[r]) =>
        withStorage('{ (storage: Storage) => (x1: t1) ?=>
          ${
            given q2: Quotes = Symbol.spliceOwner.asQuotes;
            getOrEvalExpr[r](
              q2.reflect.TypeRepr.of(using storageType.asType),
              'storage,
              List('x1),
              '{ ${ original.asExpr }.asInstanceOf[t1 ?=> r](using x1): r }
            )
          }
        })
      case (List('[t1], '[t2]), '[r]) =>
        withStorage('{ (storage: Storage) => (x1: t1, x2: t2) ?=>
          ${
            given q2: Quotes = Symbol.spliceOwner.asQuotes;
            getOrEvalExpr[r](
              q2.reflect.TypeRepr.of(using storageType.asType),
              'storage,
              List('x1, 'x2),
              '{
                ${ original.asExpr }
                  .asInstanceOf[(t1, t2) ?=> r](using x1, x2): r
              }
            )
          }
        })
      case (List('[t1], '[t2], '[t3]), '[r]) =>
        withStorage('{ (storage: Storage) => (x1: t1, x2: t2, x3: t3) ?=>
          ${
            given q2: Quotes = Symbol.spliceOwner.asQuotes;
            getOrEvalExpr[r](
              q2.reflect.TypeRepr.of(using storageType.asType),
              'storage,
              List('x1, 'x2, 'x3),
              '{
                ${ original.asExpr }
                  .asInstanceOf[(t1, t2, t3) ?=> r](using x1, x2, x3): r
              }
            )
          }
        })
      case (List('[t1], '[t2], '[t3], '[t4]), '[r]) =>
        withStorage(
          '{ (storage: Storage) => (x1: t1, x2: t2, x3: t3, x4: t4) ?=>
            ${
              given q2: Quotes = Symbol.spliceOwner.asQuotes;
              getOrEvalExpr[r](
                q2.reflect.TypeRepr.of(using storageType.asType),
                'storage,
                List('x1, 'x2, 'x3, 'x4),
                '{
                  ${ original.asExpr }.asInstanceOf[(t1, t2, t3, t4) ?=> r](
                    using
                    x1,
                    x2,
                    x3,
                    x4
                  ): r
                }
              )
            }
          }
        )
      case (List('[t1], '[t2], '[t3], '[t4], '[t5]), '[r]) =>
        withStorage(
          '{ (storage: Storage) => (x1: t1, x2: t2, x3: t3, x4: t4, x5: t5) ?=>
            ${
              given q2: Quotes = Symbol.spliceOwner.asQuotes;
              getOrEvalExpr[r](
                q2.reflect.TypeRepr.of(using storageType.asType),
                'storage,
                List('x1, 'x2, 'x3, 'x4, 'x5),
                '{
                  ${ original.asExpr }.asInstanceOf[(t1, t2, t3, t4, t5) ?=> r](
                    using
                    x1,
                    x2,
                    x3,
                    x4,
                    x5
                  ): r
                }
              )
            }
          }
        )
      case _ =>
        throw Exception(
          s"Memoization of the supplied context function with the arity of ${args.length} is not supported"
        )
  @experimental
  def memoizeRefinement(using
      q1: Quotes
  )(original: quotes.reflect.Term, storageType: quotes.reflect.TypeRepr)(
      clazz: quotes.reflect.TypeRepr,
      refinementType: quotes.reflect.PolyType | quotes.reflect.MethodType
  ): Expr[Any] =
    import quotes.reflect.*
    val closureType = original.tpe
    // We cannot create the context functions using the reflect API. We can however,
    // Do so using quotations ('{...}), so we will cover some arities of context functions to a
    // reasonable extent and a little beyond
    refinementType match
      case mt @ MethodType(_, args, rt) if closureType.isContextFunctionType =>
        val unrefined = memoizeContextFunction(original, storageType)(
          args,
          rt
        )
        return Refinement(
          unrefined.asTerm.tpe,
          "apply",
          mt
        ).asType match
          case '[t] => '{ $unrefined.asInstanceOf[t]: t }
      case _ => ()

    val parents =
      List(
        TypeTree.of[Object],
        TypeTree.of(using clazz.asType),
        TypeTree.of(using storageType.asType)
      )

    val cls = Symbol.newClass(
      Symbol.spliceOwner,
      "$anon",
      parents = parents.map(_.tpe),
      cls =>
        List(
          Symbol.newMethod(
            cls,
            "apply",
            refinementType
          )
        ),
      selfType = None
    )

    val applySym = cls.declaredMethod("apply").head
    // Currently, I don't think there is a way to get the
    // correct body type other than to do this. We cannot
    // use the `MethodType.resType` to fetch the types as
    // the type parameter references there are included as
    // `ParamRef`s instead of `TypeRef`s
    val bodyType = DefDef(applySym, _ => None).returnTpt.tpe
    val applyDef = DefDef(
      applySym,
      argss => {
        given q2: Quotes = applySym.asQuotes
        val hasTypeParams = argss.exists(args =>
          args.exists {
            case _: TypeTree => true
            case _           => false
          }
        )
        val argsIdx = if hasTypeParams then 1 else 0
        val (types, args) = argss.splitAt(argsIdx)
        val storage = This(cls).asExprOf[Storage]
        val typeApplied =
          Select
            .unique(original, "apply")
            .appliedToTypeTrees(types.flatten.asInstanceOf[List[TypeTree]])
        val applied = args.foldLeft(typeApplied) { (c, i) =>
          c.appliedToArgs(i.asInstanceOf[List[Term]])
        }
        val body = bodyType.asType match
          case '[o] =>
            getOrEvalExpr[o](
              q2.reflect.TypeRepr.of(using storageType.asType),
              storage,
              args.flatten.map(_.asExpr),
              applied.asExpr
            ).asExprOf[o]
        Some(body.asTerm)
      }
    )
    val clsDef = ClassDef(cls, parents, body = List(applyDef))
    Block(
      List(clsDef),
      Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil)
    ).asExpr

  @experimental
  private def getOrEvalExpr[O](using Quotes, Type[O])(
      storageType: quotes.reflect.TypeRepr,
      storageRef: Expr[Storage],
      args: List[Expr[Any]],
      default: Expr[Any]
  ): Expr[O] =
    import quotes.reflect.*
    val eval = memoize(
      default,
      TypeRepr.of(using storageType.asType)
    )
    '{
      $storageRef
        .getOrEval(
          ${
            if args.length == 1 then args.head else Expr.ofTupleFromSeq(args)
          },
          $eval
        )
        .asInstanceOf[O]
    }
