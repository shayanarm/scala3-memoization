package memoize

import scala.reflect.Typeable
import data.Eq
import scala.collection.mutable
import scala.quoted.*
import scala.PolyFunction
import scala.annotation.experimental
import scala.util.Success
import java.util.concurrent.Callable

abstract class Memo[A, +B]:
  def apply(using ev: Eq[A]): B

object Memoize:
  private def placeholder[A]: A = throw Exception(
    "All references to `placeholder` must be eliminated by the end of compilation. This is a bug"
  )

  private def memoType(tp: Type[_])(using q: Quotes) =
    import q.reflect.*
    tp match
      case '[a] => memoize('{ placeholder[a] }).asTerm.tpe.asType

  private def tupleType(using q: Quotes)(ts: List[Type[_]]): Type[_] =
    import q.reflect.*
    tupled(
      ts.map { case '[a] => '{ placeholder[a] } }
    ).asTerm.tpe.asType

  private def tupled(using q: Quotes)(es: List[Expr[Any]]): Expr[Any] =
    import q.reflect.*
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

  private def untupled(using q: Quotes)(e: Expr[Any]): List[Expr[Any]] =
    import q.reflect.*
    e match
      case '{ $x: EmptyTuple } => Nil
      case '{ $x: h *: t } => '{ ${ x }.head: h } :: untupled('{ $x.tail: t })
      case _               => List(e)

  def memoize(f: Expr[Any])(using q: Quotes): Expr[Any] =
    import q.reflect.*

    val widened = f.asTerm.tpe.widen
    widened.asType match
      case '[t] =>
        '{ ${ f.asExprOf[t] }: t } match
          case '{ $g: PolyFunction } => memoizePoly(g)
          case g =>
            TypeRepr.of[t] match
              case AppliedType(
                    TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), name),
                    types
                  ) if name.matches("""^Function\d*$""") =>
                val (args, returnType) = types.splitAt(types.size - 1)
                memoizeMono(g)(args, false, returnType.head)
              case AppliedType(
                    TypeRef(ThisType(TypeRef(NoPrefix(), "scala")), name),
                    types
                  ) if name.matches("""^ContextFunction\d*$""") =>
                val (args, returnType) = types.splitAt(types.size - 1)
                memoizeMono(g)(args, true, returnType.head)
              case _ => g

  private def memoizeMono(f: Expr[Any])(using
      q: Quotes
  )(
      args: List[q.reflect.TypeRepr],
      isCtxFunc: Boolean,
      rt: q.reflect.TypeRepr
  ): Expr[Any] =
    import q.reflect.*
    memoType(rt.asType) match
      case '[r] =>
        args match
          case Nil =>
            '{
              {
                lazy val result: r = ${
                  rt.asType match
                    case '[t] =>
                      memoize('{ ${ f.asExprOf[() => t] }() }).asExprOf[r]
                }
                () => result
              }
            }
          case _ =>
            tupleType(args.map(_.asType)) match
              case '[tup] =>
                val fnType =
                  if isCtxFunc then Type.of[tup ?=> r] else Type.of[tup => r]
                fnType match
                  case '[g] =>
                    '{
                      new Memo[tup, g] {
                        val memo = mutable.ArrayBuffer[(tup, r)]()
                        def apply(using ev: Eq[tup]): g =
                          ${
                            if isCtxFunc
                            then
                              '{ (arg: tup) ?=>
                                ${ monoMemoImpl[tup, r](f, 'arg, 'ev, 'memo) }
                              }.asExprOf[g]
                            else
                              '{ (arg: tup) =>
                                ${ monoMemoImpl[tup, r](f, 'arg, 'ev, 'memo) }
                              }.asExprOf[g]
                          }
                      }
                    }

  // private def rebindParams(using q: Quotes)(ptNew: q.reflect.PolyType)(in: q.reflect.TypeRepr): q.reflect.TypeRepr =
  //   import q.reflect.*
  //   def go(in: TypeRepr): TypeRepr = in match
  //     case TermRef(tpe, n) => TermRef(go(tpe), n)
  //     case _: TypeRef => in
  //     case _: ConstantType => in
  //     case SuperType(tpe1, tpe2) => SuperType(go(tpe1), go(tpe2))
  //     case Refinement(tpe1, n, tpe2) => Refinement(go(tpe1), n , go(tpe2))
  //     case AppliedType(rep, args) => Applied(TypeTree.of(using go(rep).asType), args.map(a => TypeTree.of(using go(a).asType))).tpe
  //     case AnnotatedType(tpe, term) => AnnotatedType(go(tpe), term)
  //     case AndType(l, r) => AndType(go(l), go(r))
  //     case OrType(l, r) => OrType(go(l), go(r))
  //     case MatchType(tpe1, tpe2, args) => MatchType(go(tpe1), go(tpe2), args.map(go))
  //     case ByNameType(tpe) => ByNameType(go(tpe))
  //     case ParamRef(_m, idx) => ptNew.param(idx)
  //     case _: ThisType => in
  //     case _: RecursiveThis => in
  //     case RecursiveType(tpe) => RecursiveType(_ => go(tpe))
  //     case _: MethodType => in
  //     case _: PolyType => in
  //     case _: TypeLambda => in
  //     case MatchCase(tpe1, tpe2) => MatchCase(go(tpe1), go(tpe2))
  //     case TypeBounds(tpe1, tpe2) => TypeBounds(go(tpe1), go(tpe2))
  //     case _: NoPrefix => in
  //   go(in)

  private def monoMemoImpl[I, O](
      f: Expr[Any],
      in: Expr[I],
      ev: Expr[Eq[I]],
      memo: Expr[mutable.ArrayBuffer[(I, O)]]
  )(using q: Quotes, ti: Type[I], to: Type[O]): Expr[O] =
    import q.reflect.*
    '{
      $memo.synchronized {
        $memo
          .collectFirst {
            case (i, o) if $ev.eqv(i, $in) => o
          }
          .getOrElse {
            val out: O = ${
              val fTerm = f.asTerm
              memoize(
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
              ).asExprOf[O]
            }
            $memo.append(($in, out))
            out
          }
      }
    }

  def memoizeRec[I, O](
      e: Expr[(I => O) => I => O]
  )(using q: Quotes, ta: Type[I], tb: Type[O]): Expr[Memo[I, I => O]] =
    import q.reflect.*
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

  private def memoizePoly(e: Expr[Any])(using q: Quotes): Expr[Any] =
    import q.reflect.*
    throw new Exception(
      e.asTerm.show(using Printer.TreeStructure)
    )

  // @experimental
  // def newPolyImpl(using Quotes): Expr[PolyFunction] =
  //   import quotes.reflect.*

  //   val name: String = "$anon"
  //   val parents = List(TypeTree.of[Object], TypeTree.of[PolyFunction])

  //   def decls(cls: Symbol): List[Symbol] =
  //     List(
  //       Symbol.newMethod(
  //         cls,
  //         "apply",
  //         PolyType(List("X"))(_ => List(TypeBounds.empty), polyType => {
  //           val typeParam = polyType.param(0)
  //           MethodType(List("a"))(_ => List(typeParam), _ => typeParam)
  //         })
  //       )
  //     )

  //   val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = parents.map(_.tpe), decls, selfType = None)
  //   val applySym = cls.declaredMethod("apply").head

  //   // argss=List(List(TypeTree[TypeRef(NoPrefix,type X)]), List(Ident(a)))
  //   val applyDef = DefDef(applySym, argss => Some(argss(1)(0).asInstanceOf[Term]))
  //   val clsDef = ClassDef(cls, parents, body = List(applyDef))
  //   val closure = Block(List(clsDef), Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil))
  //   closure.asExprOf[PolyFunction]
