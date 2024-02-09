import org.scalatest._
import flatspec._
import matchers._
import memoize.data.Eq
import memoize.data.Dynamic
import memoize.Memo
import scala.reflect.Typeable
import scala.reflect.TypeTest
import scala.collection.mutable.ArrayBuffer

@scala.annotation.experimental
class MemoizeSpec extends AnyFlatSpec with must.Matchers {
  protected val MinExecTime = 50
  protected def benchmark[A](e: => A): (A, Long) =
    val start = System.currentTimeMillis()
    val result: A = e
    val end = System.currentTimeMillis()
    (result, end - start)

  protected def expensive[A](a: A): A =
    Thread.sleep(MinExecTime)
    a

  // """
  // `data.Dynamic`
  // """ must "correctly safe-cast to a desired type" in {
  //   val list = List(Option("Hi"))
  //   val dyn: Dynamic = Dynamic.apply(list)
  //   dyn.cast[List[Option[String]]] mustEqual Some(list)
  //   dyn.cast[List[Option[Int]]] mustEqual None
  //   dyn.cast[Any] mustEqual Some(list)
  // }
  
  // """
  // `memoize.apply`
  // """ must "memoize a simple identity function with the type parameter applied" in {
  //   val f = memoize(expensive[Int])
  //   val (r1, t1) = benchmark(f(1))
  //   val (r2, t2) = benchmark(f(1))
  //   r1 mustEqual r2
  //   assert(t2 < t1)
  // }

  // it must "memoize a curried function" in {
  //   val f =
  //     memoize(((a: Int) => (b: String, c: Char) => expensive(a, b, c)))
  //   val (r1, t1) = benchmark(f(42)("foo", 'c'))
  //   val (r2, t2) = benchmark(f(42)("foo", 'c'))
  //   r1 mustEqual r2
  //   assert(t2 < t1)
  // }

  // it must "memoize a function with dependent arguments" in {
  //   val f =
  //     memoize(((a: Int) => (b: a.type) => expensive(a, b)))
  //   val (r1, t1) = benchmark(f(42)(42))
  //   val (r2, t2) = benchmark(f(42)(42))
  //   r1 mustEqual r2
  //   assert(t2 < t1)
  // }

  // it must "allow comparison of arguments by a customaly supplied `Eq` instance" in {
  //   val f = memoize(identity[Int])

  //   given passEq[A]: Eq[A] = (_, _) => true

  //   f(using passEq)(1) mustEqual f(using passEq)(2)
  // }

  // it must "memoize implicit arguments the same way it does for normal arguments" in {
  //   val f = memoize(((x: Int) => (y: Int) ?=> expensive(x + y)))
  //   given Int = 5
  //   val (r1: Int, t1) = benchmark(f(1).apply)
  //   val (r2: Int, t2) = benchmark(f(1).apply)
  //   r1 mustEqual r2
  //   assert(t2 < t1)
  // }

  // it must "memoize a Function0" in {
  //   val f = memoize(() => expensive(42))
  //   val (r1, t1) = benchmark(f())
  //   val (r2, t2) = benchmark(f())
  //   r1 mustEqual r2
  //   assert(t2 < t1)
  // }

  // it must "memoize a polymorphic function value if their type parameters are applied upon memoization" in {
  //   val poly1 = [X, Y] => (x: X, y: Y) => x == y
  //   val f = memoize(poly1[Int, Int])
  //   f(1, 1) mustEqual poly1(1, 1)

  //   val poly2 = [X] => (x: X) => [Y] => (y: Y) => x == y
  //   val g = memoize(poly2(_: Int)[Int])
  //   g(1)(1) mustEqual poly2(1)(1)
  // }

  // it must "memoize a polymorphic identity function" in {
  //   val f = memoize([X] => (x: X) => expensive(x))
  //   val (r1, t1) = benchmark(f[Int](1))
  //   val (r2, t2) = benchmark(f[String]("foo"))
  //   val (r3, t3) = benchmark(f[Int](1))
  //   val (r4, t4) = benchmark(f[String]("foo"))
  //   r1 mustEqual r3
  //   r2 mustEqual r4
  //   assert(t3 < t1)
  //   assert(t4 < t2)
  // }

  // it must "memoize a polymorphic binary function" in {
  //   val f = memoize([X, Y] => (x: X, y: Y) => expensive((x, y)))
  //   val (r1, t1) = benchmark(f[Int, String](1, "bar"))
  //   val (r2, t2) = benchmark(f[String, Int]("foo", 42))
  //   val (r3, t3) = benchmark(f[Int, String](1, "bar"))
  //   val (r4, t4) = benchmark(f[String, Int]("foo", 42))
  //   r1 mustEqual r3
  //   r2 mustEqual r4
  //   assert(t3 < t1)
  //   assert(t4 < t2)
  // }
  
  // it must "memoize a curried polymorphic function" in {
  //   val f = memoize([X, Y, Z] => (x: X) => (y: Y, z: Z) => expensive((x, y, z)))
  //   val (r1, t1) = benchmark(f[Int, String, Char](1)("foo", 'c'))
  //   val (r2, t2) = benchmark(f[Boolean, Double, Int](false)(5d, 3))
  //   val (r3, t3) = benchmark(f[Int, String, Char](1)("foo", 'c'))
  //   val (r4, t4) = benchmark(f[Boolean, Double, Int](false)(5d, 3))
  //   r1 mustEqual r3
  //   r2 mustEqual r4
  //   assert(t3 < t1)
  //   assert(t4 < t2)
  // }

  // it must "memoize a polymorphic function nested inside a monomorphic function" in {
  //   val f = memoize((x: Int) => [Y, Z] => (y: Y, z: Z) => expensive((x, y, z)))
  //   val (r1, t1) = benchmark(f(1)("foo", 'c'))
  //   val (r2, t2) = benchmark(f(1)(5d, 3))
  //   val (r3, t3) = benchmark(f(1)("foo", 'c'))
  //   val (r4, t4) = benchmark(f(1)(5d, 3))
  //   r1 mustEqual r3
  //   r2 mustEqual r4
  //   assert(t3 < t1)
  //   assert(t4 < t2)
  // }

  // it must "memoize a polymorphic function with dependent arguments" in {
  //   val f = 
  //     memoize(([X] => (a: X) => (b: a.type) => expensive(a, b)))
  //   val (r1, t1) = benchmark(f[Int](42)(42))
  //   val (r2, t2) = benchmark(f[Int](42)(42))
  //   r1 mustEqual r2
  //   assert(t2 < t1)
  // }
  
  // it must "memoize a polymorphic function with a type parameter bounded by another type parameter" in {
  //   case class Base()
  //   class Extended extends Base()
  //   val f = memoize([A, B <: A] => (a: A, b: B) => expensive((a, b)))
  //   val (r1, t1) = benchmark(f(Base(), Extended()))
  //   val (r2, t2) = benchmark(f(Base(), Extended()))
  //   r1 mustEqual r2
  //   assert(t2 < t1)
  // }

  it must "memoize nested polymorphic functions" in {
    val f = memoize([X] => (x: X) => [Y] => (y: Y) => expensive((x, y)))
    val (r1, t1) = benchmark(f(1)("foo"))
    val (r2, t2) = benchmark(f(1)(5L))
    val (r3, t3) = benchmark(f(1)("foo"))
    val (r4, t4) = benchmark(f(1)(5L))
    r1 mustEqual r3
    r2 mustEqual r4
    assert(t3 < t1)
    assert(t4 < t2)
  }  

  // it must "memoize a polymorphic function with a higher-kinded typa parameter" in {
  //   val f = memoize([A >: Nothing <: Any, M[_ >: Nothing <: A]] => (arg: M[A], b: arg.type) => expensive(arg))
  //   val arg13 = Some(42)
  //   val arg24 = List(42)
  //   val (r1, t1) = benchmark(f[Int, Option](arg13, arg13))
  //   val (r2, t2) = benchmark(f[Int, List](arg24, arg24))
  //   val (r3, t3) = benchmark(f[Int, Option](arg13, arg13))
  //   val (r4, t4) = benchmark(f[Int, List](arg24, arg24))
  //   r1 mustEqual r3
  //   r2 mustEqual r4
  //   assert(t3 < t1)
  //   assert(t4 < t2)
  // }  

  // """
  // `memoize.define`
  // """ must "memoize a recursively defined function" in {
  //   val f = memoize.define[Long, Long] { f => n =>
  //     if n == 0 then n else expensive(f(0))
  //   }

  //   val (r1, t1) = benchmark(f(1))
  //   val (r2, t2) = benchmark(f(1))
  //   r1 mustEqual r2
  //   assert(t2 < t1)
  // }

  // it must "be reliably faster to compute the fibonacci sequence on large inputs" in {

  //   def fibb(n: Long): Long =
  //     n match
  //       case 0 => 0
  //       case 1 => 1
  //       case n if n < 0 =>
  //         throw ArithmeticException()
  //       case _ => fibb(n - 1) + fibb(n - 2)

  //   val memoFibb = memoize.define[Long, Long] { f => n =>
  //     n match
  //       case 0 => 0
  //       case 1 => 1
  //       case n if n < 0 =>
  //         throw ArithmeticException()
  //       case _ => f(n - 1) + f(n - 2)
  //   }

  //   (35 to 40) foreach { n =>
  //     val (r1, t1) = benchmark { fibb(n) }
  //     val (r2, t2) = benchmark { memoFibb(n) }
  //     assert(r1 == r2)
  //     assert(t2 < t1)
  //   }
  // }
}
