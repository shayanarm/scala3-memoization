package memoize

import scala.reflect.Manifest
import data.Eq
import scala.collection.mutable
import scala.quoted.*
import scala.PolyFunction
import scala.annotation.experimental
import scala.util.Success
import java.util.concurrent.Callable
import scala.util.DynamicVariable
import scala.collection.mutable.HashMap

abstract class Memo[A, +B]:
  def apply(using ev: Eq[A]): B
class Temp
object Memoize:
  object Functions:
    object Mono:
      def unapply(using
          Quotes
      )(
          tpe: quotes.reflect.TypeRepr
      ): Option[(List[quotes.reflect.TypeRepr], quotes.reflect.TypeRepr)] =
        import quotes.reflect.*
        tpe match
          case f @ AppliedType(
                TypeRef(_, name),
                types
              ) if f.isFunctionType =>
            val (args, returnType) = types.splitAt(types.size - 1)
            Some((args, returnType.head))
          case _ => None
    object Poly:
      def unapply(using Quotes)(
          tpe: quotes.reflect.TypeRepr
      ): Option[quotes.reflect.PolyType] =
        import quotes.reflect.*
        tpe match
          case Refinement(
                tr,
                "apply",
                pt: PolyType
              ) if tr =:= TypeRepr.of[PolyFunction] =>
            Some(pt)
          case _ => None

  private def placeholder[A]: A = throw Exception(
    "All references to `placeholder` must be eliminated by the end of macro execution. This is a bug"
  )

  @experimental
  private def memoType(using Quotes)(
      tp: quotes.reflect.TypeRepr
  ): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    tp match
      case Functions.Poly(
            pt @ PolyType(
              _,
              _,
              mtOld1: MethodType
            )
          ) =>
        transformTypeRepr({
          case mtOld @ MethodType(paramNames, paramTypes, returnType)
              if mtOld1.paramNames == paramNames =>
            MethodType(paramNames)(
              mt =>
                paramTypes
                  .map(rebindParams((mtOld, mt))),
              mt => {
                (
                  tupleType(
                    paramTypes
                      .map(rebindParams((mtOld, mt)))
                  ).asType,
                  (rebindParams(
                    (mtOld, mt)
                  ) andThen memoType)(returnType).asType
                ) match
                  case ('[tup], '[r]) =>
                    TypeRepr.of[(Manifest[tup], Eq[tup]) ?=> r]
              }
            )
        })(tp)
      case Functions.Mono(args, rt) =>
        args match
          case Nil =>
            memoType(rt).asType match
              case '[r] => TypeRepr.of[() => r]
          case _ =>
            (tupleType(args).asType, memoType(rt).asType) match
              case ('[tup], '[r]) if tp.isContextFunctionType =>
                TypeRepr.of[Memo[tup, tup ?=> r]]
              case ('[tup], '[r]) => TypeRepr.of[Memo[tup, tup => r]]
      case _ => tp

  private def tupleType(using Quotes)(
      ts: List[quotes.reflect.TypeRepr]
  ): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    tupled(
      ts.map(_.asType).map { case '[a] => '{ placeholder[a]: a } }
    ).asTerm.tpe

  private def tupled(using Quotes)(es: List[Expr[Any]]): Expr[Any] =
    import quotes.reflect.*
    es match
      case Nil          => '{ EmptyTuple }
      case List(single) => single
      case _ =>
        es.foldRight[Expr[_ <: Tuple]]('{ EmptyTuple }) {
          case ('{ $h: h }, c @ '{ $t: t }) =>
            Typed(
              '{ ($h *: $c) }.asTerm,
              Applied(TypeTree.of[*:], List(TypeTree.of[h], TypeTree.of[t]))
            ).asExprOf[Tuple]
        }

  private def untupled(using Quotes)(e: Expr[Any]): List[Expr[Any]] =
    import quotes.reflect.*
    e match
      case '{ $x: EmptyTuple } => Nil
      case '{ $x: h *: t } => '{ ${ x }.head: h } :: untupled('{ $x.tail: t })
      case _               => List(e)

  private def foldTypeRepr[A](using Quotes)(
      f: PartialFunction[quotes.reflect.TypeRepr, A]
  )(in: quotes.reflect.TypeRepr): List[A] =
    import quotes.reflect.*
    inline def go(a: quotes.reflect.TypeRepr): List[A] =
      foldTypeRepr(f)(a)
    val out = in match
      case TermRef(tpe, n)           => go(tpe)
      case TypeRef(tpe, name)        => Nil
      case _: ConstantType           => Nil
      case SuperType(tpe1, tpe2)     => go(tpe1) ++ go(tpe2)
      case Refinement(tpe1, n, tpe2) => go(tpe1) ++ go(tpe2)
      case AppliedType(rep, args) =>
        go(rep) ++ args.map(go(_)).flatten
      case AnnotatedType(tpe, term) => go(tpe)
      case AndType(l, r)            => go(l) ++ go(r)
      case OrType(l, r)             => go(l) ++ go(r)
      case MatchType(tpe1, tpe2, args) =>
        go(tpe1) ++ go(tpe2) ++ args.map(go(_)).flatten
      case ByNameType(tpe)              => go(tpe)
      case ParamRef(_m, idx)            => Nil
      case _: ThisType                  => Nil
      case _: RecursiveThis             => Nil
      case RecursiveType(tpe)           => go(tpe)
      case MethodType(params, args, rt) => args.map(go(_)).flatten ++ go(rt)
      case PolyType(params, bounds, rt) => bounds.map(go(_)).flatten ++ go(rt)
      case TypeLambda(_, bounds, body)  => bounds.map(go(_)).flatten ++ go(body)
      case MatchCase(tpe1, tpe2)        => go(tpe1) ++ go(tpe2)
      case TypeBounds(tpe1, tpe2)       => go(tpe1) ++ go(tpe2)
      case _: NoPrefix                  => Nil
      case tree => throw MatchError(tree.show(using Printer.TypeReprStructure))

    f.andThen(_ :: out).applyOrElse(in, _ => out)

  private def transformTypeRepr(using Quotes)(
      f: PartialFunction[quotes.reflect.TypeRepr, quotes.reflect.TypeRepr]
  )(in: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    inline def go(a: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
      transformTypeRepr(f)(a)
    val out = in match
      case TermRef(tpe, n) =>
        TermRef(go(tpe), n)
      case TypeRef(tpe, name)        => in
      case _: ConstantType           => in
      case SuperType(tpe1, tpe2)     => SuperType(go(tpe1), go(tpe2))
      case Refinement(tpe1, n, tpe2) => Refinement(go(tpe1), n, go(tpe2))
      case AppliedType(rep, args) =>
        AppliedType(
          go(rep),
          args.map(go(_))
        )
      case AnnotatedType(tpe, term) => AnnotatedType(go(tpe), term)
      case AndType(l, r)            => AndType(go(l), go(r))
      case OrType(l, r)             => OrType(go(l), go(r))
      case MatchType(tpe1, tpe2, args) =>
        MatchType(go(tpe1), go(tpe2), args.map(go(_)))
      case ByNameType(tpe)       => ByNameType(go(tpe))
      case ParamRef(binder, idx) => in
      case _: ThisType           => in
      case _: RecursiveThis      => in
      case old @ RecursiveType(tpe) =>
        RecursiveType(rec => go(rebindParams((old, rec))(tpe)))
      case old @ MethodType(ns, args, rt) =>
        MethodType(ns)(
          mt => args.map(a => go(rebindParams((old, mt))(a))),
          mt => go(rebindParams((old, mt))(rt))
        )
      case old @ PolyType(params, bounds, rt) =>
        PolyType(params)(
          pt =>
            bounds.map(b =>
              go(rebindParams((old, pt))(b)).asInstanceOf[TypeBounds]
            ),
          pt => go(rebindParams((old, pt))(rt))
        )
      case old @ TypeLambda(params, bounds, body) =>
        TypeLambda(
          params,
          tl =>
            bounds.map(b =>
              go(rebindParams((old, tl))(b)).asInstanceOf[TypeBounds]
            ),
          tl => go(rebindParams((old, tl))(body))
        )
      case MatchCase(tpe1, tpe2)  => MatchCase(go(tpe1), go(tpe2))
      case TypeBounds(tpe1, tpe2) => TypeBounds(go(tpe1), go(tpe2))
      case _: NoPrefix            => in
      case tree => throw MatchError(tree.show(using Printer.TypeReprStructure))
    f.applyOrElse(out, identity)

  private def rebindParams(using
      Quotes
  )(
      pair: (quotes.reflect.PolyType, quotes.reflect.PolyType) |
        (quotes.reflect.MethodType, quotes.reflect.MethodType) |
        (quotes.reflect.TypeLambda, quotes.reflect.TypeLambda) |
        (quotes.reflect.RecursiveType, quotes.reflect.RecursiveType)
  )(
      in: quotes.reflect.TypeRepr
  ): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    // val g: PartialFunction[TypeRepr, TypeRepr] = {
    //   case t =>
    //     println(s"${t.show(using Printer.TypeReprAnsiCode)}")
    //     t
    // }
    val f: PartialFunction[TypeRepr, TypeRepr] = pair match
      case (old, neo: PolyType) => {
        case ref @ ParamRef(binder, idx) if binder == old =>
          neo.param(idx)
      }
      case (old, neo: MethodType) => {
        case ref @ ParamRef(binder, idx) if binder == old =>
          neo.param(idx)
      }

      case (old: TypeLambda, neo: TypeLambda) => {
        case ref @ ParamRef(binder, idx) if binder == old =>
          neo.param(idx)

      }
      case (
            old: RecursiveType,
            neo: RecursiveType
          ) => {
        case rec if rec == old =>
          neo
      }

    transformTypeRepr( /*g andThen */ f)(in)

  @experimental
  def memoize(expr: Expr[Any])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    val widened = expr.asTerm.tpe.widen.asType match
      case '[w] => '{ ${ expr.asExprOf[w] }: w }

    // val before = '{
    //   new PolyFunction {
    //     var memo: Int = 5
    //     def apply[X](x: X): Boolean = memo == x
    //   }
    // }.asTerm.tpe
    // val after = memoType(before)

    // println(s"before : ${before.show(using Printer.TypeReprShortCode)}")
    // println(s"after : ${after.show(using Printer.TypeReprShortCode)}")

    widened.asTerm.tpe match
      case Functions.Poly(pt)       => memoizePoly(widened)(pt)
      case Functions.Mono(args, rt) => memoizeMono(widened)(args, rt)
      case _                        => expr

  @experimental
  private def memoizeMono(f: Expr[Any])(using
      Quotes
  )(
      args: List[quotes.reflect.TypeRepr],
      rt: quotes.reflect.TypeRepr
  ): Expr[Any] =
    import quotes.reflect.*
    memoType(f.asTerm.tpe).asType match
      case '[() => t] =>
        '{
          {
            lazy val result = ${
              memoize('{ ${ f.asExprOf[() => t] }() }).asExprOf[t]
            }
            () => result
          }
        }
      case '[Memo[tup, _ ?=> r]] =>
        '{
          new Memo[tup, tup ?=> r] {
            private val memo = mutable.ArrayBuffer[(tup, r)]()
            def apply(using ev: Eq[tup]): tup ?=> r = (arg: tup) ?=>
              ${ monoMemoImpl[tup, r](f, 'arg, 'ev, 'memo).asExprOf[r] }
          }
        }
      case '[Memo[tup, _ => r]] =>
        '{
          new Memo[tup, tup => r] {
            private val memo = mutable.ArrayBuffer[(tup, r)]()
            def apply(using ev: Eq[tup]): tup => r = (arg: tup) =>
              ${ monoMemoImpl[tup, r](f, 'arg, 'ev, 'memo).asExprOf[r] }
          }
        }

  @experimental
  private def monoMemoImpl[I, O](
      f: Expr[Any],
      in: Expr[I],
      ev: Expr[Eq[I]],
      memo: Expr[mutable.ArrayBuffer[(I, O)]]
  )(using Quotes)(using ti: Type[I], to: Type[O]): Expr[O] =
    import quotes.reflect.*
    '{
      $memo.synchronized {
        $memo
          .collectFirst {
            case (i, o) if $ev.eqv(i, $in) => o
          }
          .getOrElse {
            val out: O = ${
              val fTerm = f.asTerm
              val result = memoize(
                fTerm
                  .select(
                    fTerm.tpe.classSymbol.get
                      .declaredMethod("apply")
                      .head
                  )
                  .appliedToArgs(
                    untupled(in).map(_.asTerm)
                  )
                  .asExpr
              )
              result.asExprOf[O]
            }
            $memo.append(($in, out))
            out
          }
      }
    }

  def memoizeRec[I, O](
      e: Expr[(I => O) => I => O]
  )(using Quotes)(using ta: Type[I], tb: Type[O]): Expr[Memo[I, I => O]] =
    import quotes.reflect.*
    Expr.betaReduce('{ $e(placeholder[I => O]) }) match
      case '{ $d: (I => O) } =>
        '{
          new Memo[I, I => O] {
            val memo = mutable.ArrayBuffer[(I, O)]()
            def apply(using ev: Eq[I]): I => O =
              new Function[I, O] { self =>
                def apply(in: I): O =
                  self.synchronized {
                    memo
                      .collectFirst {
                        case (i, o) if ev.eqv(i, in) => o
                      }
                      .getOrElse {
                        val out: O = ${
                          new TreeMap {
                            override def transformTerm(
                                tree: Term
                            )(owner: Symbol): Term =
                              val t = super.transformTerm(tree)(owner)
                              if t.isExpr then
                                t.asExpr match
                                  case '{ placeholder[I => O] } =>
                                    '{ self.apply }.asTerm
                                  case _ => t
                              else t
                          }.transformTerm(d.asTerm)(Symbol.spliceOwner)
                            .asExprOf[I => O]
                        }.apply(in)
                        memo.append((in, out))
                        out
                      }
                  }
              }
          }
        }

  @experimental
  def memoizePoly(using q1: Quotes)(original: Expr[Any])(
      ptOld: quotes.reflect.PolyType
  ): Expr[Any] =
    import quotes.reflect.*

    val name: String = "$anon"
    val parents = List(TypeTree.of[Object], TypeTree.of[PolyFunction])
    val closureType = memoType(original.asTerm.tpe)
    val Functions.Poly(poly) = closureType

    val cls = Symbol.newClass(
      Symbol.spliceOwner,
      name,
      parents = parents.map(_.tpe),
      cls =>
        List(
          Symbol.newVal(
            cls,
            "memo",
            TypeRepr.of[mutable.ArrayBuffer[(data.Dynamic, Any)]],
            Flags.EmptyFlags,
            cls
          ),
          Symbol.newMethod(
            cls,
            "apply",
            poly
          )
        ),
      selfType = None
    )

    val applySym = cls.declaredMethod("apply").head
    val memoSym = cls.declaredField("memo")
    val memoDef = ValDef(
      memoSym,
      Some('{ mutable.ArrayBuffer[(data.Dynamic, Any)]() }.asTerm)
    )

    // Currently, there is not way to get the correct body type other than to do this.
    val bodyType = DefDef(applySym, _ => None) match
      case DefDef(_, _, rt, _) => rt.tpe

    val applyDef = DefDef(
      applySym,
      argss => {
        given q2: Quotes = applySym.asQuotes
        val memo =
          Select(This(cls), memoSym)
            .asExprOf[mutable.ArrayBuffer[(data.Dynamic, Any)]]
        val tupledArgs = tupled(
          argss(1).map(_.asExpr)
        )
        val body =
          bodyType.asType match
            case '[(Manifest[i], _) ?=> o] =>
              '{ (tpbl: Manifest[i], eqlt: Eq[i]) ?=>
                // val in = ${ tupledArgs.asExprOf[i] }
                // $memo.synchronized {
                //   $memo
                //     .collectFirst {
                //       case (i, out)
                //           if i
                //             .cast[i]
                //             .exists(i => eqlt.eqv(i, in)) =>
                //         out.asInstanceOf[o]
                //     }
                //     .getOrElse {
                      val result: o = ${
                        memoize(
                          Select
                            .unique(original.asTerm, "apply")
                            .appliedToTypes(
                              argss(0).map(_.asInstanceOf[TypeTree].tpe)
                            )
                            .appliedToArgs(argss(1).map(_.asInstanceOf[Term]))
                            .asExpr
                        ).asExprOf[o]
                      }
                      // $memo.append(
                      //   (data.Dynamic.apply[i](in), result)
                      // )
                      result
                //     }
                // }
              }.asExprOf[(Manifest[i], Eq[i]) ?=> o]
        Some(body.asTerm)
      }
    )
    val clsDef = ClassDef(cls, parents, body = List(memoDef, applyDef))
    val closure =
      Block(
        List(clsDef),
        Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil)
      )
    closureType.asType match
      case '[t] => closure.asExprOf[t]
