package memoize.data

import scala.quoted.*
import scala.reflect.*

trait Eq[A]:
  def eqv(a: A, b: A): Boolean

object Eq extends LowPriorityGivens:
  given eqType[A](using q: Quotes): Eq[Type[A]] with
    def eqv(ta: Type[A], tb: Type[A]): Boolean =
      import q.reflect.*
      TypeRepr.of(using ta) =:= TypeRepr.of(using tb)

  given eqOpt[A](using eqA: Eq[A]): Eq[Option[A]] with
    def eqv(ta: Option[A], tb: Option[A]): Boolean =
      ta.zip(tb).forall(eqA.eqv)

  given eqList[A](using eqA: Eq[A]): Eq[List[A]] with
    def eqv(ta: List[A], tb: List[A]): Boolean =
      ta.length == tb.length && ta.zip(tb).forall(eqA.eqv)     
  
  given eqSeq[A](using eqA: Eq[A]): Eq[Seq[A]] with
    def eqv(ta: Seq[A], tb: Seq[A]): Boolean =
      ta.length == tb.length && ta.zip(tb).forall(eqA.eqv)     

  given eqIndexedSeq[A](using eqA: Eq[A]): Eq[IndexedSeq[A]] with
    def eqv(ta: IndexedSeq[A], tb: IndexedSeq[A]): Boolean =
      ta.length == tb.length && ta.zip(tb).forall(eqA.eqv)          

  given eqArray[A](using eqA: Eq[A]): Eq[Array[A]] with
    def eqv(ta: Array[A], tb: Array[A]): Boolean =
      ta.length == tb.length && ta.zip(tb).forall(eqA.eqv)

  given eqVector[A](using eqA: Eq[A]): Eq[Vector[A]] with
    def eqv(ta: Vector[A], tb: Vector[A]): Boolean =
      ta.length == tb.length && ta.zip(tb).forall(eqA.eqv)    

  given eqLazyList[A](using eqA: Eq[A]): Eq[LazyList[A]] with
    def eqv(ta: LazyList[A], tb: LazyList[A]): Boolean =
      ta.length == tb.length && ta.zip(tb).forall(eqA.eqv)    

  given tupleBase: Eq[EmptyTuple] with
    def eqv(ta: EmptyTuple, tb: EmptyTuple): Boolean = ta == tb

  given tupleInductive[H, T <: Tuple](using evH: Eq[H], evT: Eq[T]): Eq[H *: T]
    with
    def eqv(ta: H *: T, tb: H *: T): Boolean =
      evH.eqv(ta.head, tb.head) && evT.eqv(ta.tail, tb.tail)

  given eqOr[A: Typeable, B: Typeable](using eqA: Eq[A], eqB: Eq[B]): Eq[A | B]
    with
    def eqv(ta: A | B, tb: A | B): Boolean = (ta, tb) match
      case (a: A, b: A) => eqA.eqv(a, b)
      case (a: B, b: B) => eqB.eqv(a, b)
      case _            => false
      
  given eqAnd[A: Typeable, B: Typeable](using eqA: Eq[A], eqB: Eq[B]): Eq[A & B]
    with
    def eqv(ta: A & B, tb: A & B): Boolean =
      eqA.eqv(ta, tb) && eqB.eqv(ta, tb)

trait LowPriorityGivens:
  given default[A]: Eq[A] with
    def eqv(ta: A, tb: A): Boolean =
      ta == tb
