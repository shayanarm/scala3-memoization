import org.scalatest._
import flatspec._
import matchers._
import memoize.data.Eq
import memoize.Memo
import scala.reflect.Typeable

@scala.annotation.experimental
class MemoizeSpec extends AnyFlatSpec with must.Matchers {
  protected val MinExecTime = 100
  protected def benchmark[A](e: => A): (A, Long) =
    val start = System.currentTimeMillis()
    val result: A = e
    val end = System.currentTimeMillis()
    (result, end - start)

  protected def expensive[A](a: A): A =
    Thread.sleep(MinExecTime)
    a

  // """
  // `memoize.apply`
  // """ must "memoize a simple identity function with the type parameter applied" in {
  //   val f = memoize(expensive compose identity[Int])
  //   val (r1, t1) = benchmark(f(1))
  //   val (r2, t2) = benchmark(f(1))
  //   r1 mustEqual r2
  //   assert(t2 < t1)
  // }

  // it must "memoize a curried function" in {
  //   val f =
  //     memoize(((a: Int) => (b: String, c: Char) => expensive(a, b, c)))
  //   val (r1, t1) = benchmark(f(1))
  //   val (r2, t2) = benchmark(f(1))
  //   r1 mustEqual r2
  //   assert(t2 < t1)
  // }

  // it must "allow comparison of arguments by a customaly supplied `Eq` instance" in {
  //   val f = memoize(identity[Int])

  //   given passEq[A]: Eq[A] = (_, _) => true

  //   f(using passEq)(1) mustEqual f(using passEq)(2)
  // }

  // it must "memoize implicit arguments the same way it does for normal arguments" in {
  //   val f = memoize(expensive compose ((x: Int) => (y: Int) ?=> x + y))
  //   given Int = 5
  //   val (r1, t1) = benchmark(f(1))
  //   val (r2, t2) = benchmark(f(1))
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

  // it must "memoize a Function0" in {
  //   val f = memoize(() => expensive(42))
  //   val (r1, t1) = benchmark(f())
  //   val (r2, t2) = benchmark(f())
  //   r1 mustEqual r2
  //   assert(t2 < t1)
  // }

  it must "memoize a polymorphic identity function" in {
    val f = memoize([X] => (x: List[Option[X]]) => expensive(x))
    val (r1, t1) = benchmark(f(List(Some(1))))
    val (r2, t2) = benchmark(f(List(Some(1))))
    r1 mustEqual r2
    assert(t2 < t1)
  }

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
