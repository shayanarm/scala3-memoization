import memoization._
import org.scalatest._
import flatspec._
import matchers._
import scala.reflect.Typeable
import scala.reflect.TypeTest
import scala.collection.mutable.ArrayBuffer
import scala.quoted.*
import scala.collection.mutable.WeakHashMap
import scala.collection.mutable.HashMap
import org.scalactic.Bool

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

  private def fib(n: Long): Long =
    n match
      case 0 | 1 => n
      case n if n < 0 =>
        throw ArithmeticException()
      case _ => fib(n - 1) + fib(n - 2)

  """
  `f.memoized`
  """ must "memoize a simple identity function with the type parameter applied" in {
    val f = expensive[Int].memoized
    val (r1, t1) = benchmark(f(1))
    val (r2, t2) = benchmark(f(1))
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize a curried function" in {
    val f: (Int => (String, Char) => (Int, String, Char)) =
      (((a: Int) => (b: String, c: Char) => expensive(a, b, c))).memoized
    val (r1, t1) = benchmark(f(42)("foo", 'c'))
    val (r2, t2) = benchmark(f(42)("foo", 'c'))
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize context functions the same way it does for normal functions" in {
    val f: Int => Int ?=> Int = (((x: Int) => (y: Int) ?=> expensive(x + y))).memoized
    given Int = 5
    val (r1: Int, t1) = benchmark(f(1).apply)
    val (r2: Int, t2) = benchmark(f(1).apply)
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize a Function0" in {
    val f = (() => expensive(42)).memoized
    val (r1, t1) = benchmark(f())
    val (r2, t2) = benchmark(f())
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize a monomorphic function with dependent arguments" in {
    val x = 42
    val f =
      (((a: x.type) => (b: Int) ?=> (c: b.type) => expensive((a, b)))).memoized
    val (r1, t1) = benchmark(f(x)(using 15)(15))
    val (r2, t2) = benchmark(f(x)(using 15)(15))
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize a polymorphic identity function" in {
    val f = ([X] => (x: X) => expensive(x)).memoized
    val (r1, t1) = benchmark(f[Int](1))
    val (r2, t2) = benchmark(f[String]("foo"))
    val (r3, t3) = benchmark(f[Int](1))
    val (r4, t4) = benchmark(f[String]("foo"))
    r1 mustEqual r3
    r2 mustEqual r4
    assert(t3 < t1)
    assert(t4 < t2)
  }

  it must "memoize a polymorphic binary function" in {
    val f = ([X, Y] => (x: X, y: Y) => expensive((x, y))).memoized
    val (r1, t1) = benchmark(f[Int, String](1, "bar"))
    val (r2, t2) = benchmark(f[String, Int]("foo", 42))
    val (r3, t3) = benchmark(f[Int, String](1, "bar"))
    val (r4, t4) = benchmark(f[String, Int]("foo", 42))
    r1 mustEqual r3
    r2 mustEqual r4
    assert(t3 < t1)
    assert(t4 < t2)
  }

  it must "memoize a curried polymorphic function" in {
    val f =
      ([X, Y, Z] => (x: X) => (y: Y, z: Z) => expensive((x, y, z))).memoized
    val (r1, t1) = benchmark(f[Int, String, Char](1)("foo", 'c'))
    val (r2, t2) = benchmark(f[Boolean, Double, Int](false)(5d, 3))
    val (r3, t3) = benchmark(f[Int, String, Char](1)("foo", 'c'))
    val (r4, t4) = benchmark(f[Boolean, Double, Int](false)(5d, 3))
    r1 mustEqual r3
    r2 mustEqual r4
    assert(t3 < t1)
    assert(t4 < t2)
  }

  it must "memoize a polymorphic function nested inside a monomorphic function" in {
    val f = ((x: Int) => [Y, Z] => (y: Y, z: Z) => expensive((x, y, z)))
      .memoized(using HashMap)
    val (r1, t1) = benchmark(f(1)("foo", 'c'))
    val (r2, t2) = benchmark(f(1)(5d, 3))
    val (r3, t3) = benchmark(f(1)("foo", 'c'))
    val (r4, t4) = benchmark(f(1)(5d, 3))
    r1 mustEqual r3
    r2 mustEqual r4
    assert(t3 < t1)
    assert(t4 < t2)
  }

  it must "memoize a polymorphic function with dependent arguments" in {
    val f =
      (([X] => (a: X) => (b: a.type) => expensive(a, b))).memoized
    val (r1, t1) = benchmark(f[Int](42)(42))
    val (r2, t2) = benchmark(f[Int](42)(42))
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize a polymorphic function with a type parameter bounded by another type parameter" in {
    case class Base()
    class Extended extends Base()
    val f = ([A, B <: A] => (a: A, b: B) => expensive((a, b))).memoized
    val (r1, t1) = benchmark(f(Base(), Extended()))
    val (r2, t2) = benchmark(f(Base(), Extended()))
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize nested polymorphic functions" in {
    val f = ([X] => (x: X) => [Y] => (y: Y) => expensive((x, y))).memoized
    val (r1, t1) = benchmark(f(1)("foo"))
    val (r2, t2) = benchmark(f(1)(5L))
    val (r3, t3) = benchmark(f(1)("foo"))
    val (r4, t4) = benchmark(f(1)(5L))
    r1 mustEqual r3
    r2 mustEqual r4
    assert(t3 < t1)
    assert(t4 < t2)
  }

  it must "memoize a polymorphic function with a higher-kinded type parameter" in {
    val f = (
        [
            A,
            M[_ <: A]
        ] => (arg: M[A], b: arg.type) => expensive(arg)
    ).memoized
    val arg13 = Some(42)
    val arg24 = List(42)
    val (r1, t1) = benchmark(f[Int, Option](arg13, arg13))
    val (r2, t2) = benchmark(f[Int, List](arg24, arg24))
    val (r3, t3) = benchmark(f[Int, Option](arg13, arg13))
    val (r4, t4) = benchmark(f[Int, List](arg24, arg24))
    r1 mustEqual r3
    r2 mustEqual r4
    assert(t3 < t1)
    assert(t4 < t2)
  }

  it must "memoize a polymorphic context function (Wow! Who does that?!)" in {
    val f = ([X] => (x: X) ?=> expensive(x)).memoized
    val (r1, t1) = benchmark(f[Int](using 5))
    val (r2, t2) = benchmark(f[String](using "foo"))
    val (r3, t3) = benchmark(f[Int](using 5))
    val (r4, t4) = benchmark(f[String](using "foo"))
    r1 mustEqual r3
    r2 mustEqual r4
    assert(t3 < t1)
    assert(t4 < t2)
  }
  it must "memoize a recursively defined function lazy val" in {
    lazy val f: Long => Long = { (n: Long) =>
      if n == 0 then n else expensive(f(0))
    }.memoized

    val (r1, t1) = benchmark(f(1))
    val (r2, t2) = benchmark(f(1))
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize a recursively defined polymorphic function lazy val" in {
    lazy val f: [X] => X => Boolean => X = {
      [X] => (n: X) => (b: Boolean) => if b then f(n)(false) else expensive(n)
    }.memoized

    val (r1, t1) = benchmark(f(1)(false))
    val (r2, t2) = benchmark(f(1)(false))
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "convert the time complexity of the recursively defined fibonacci function to O(n)" in {

    (35 to 40) foreach { n =>
      var timesCalculated = 0
      lazy val memoFib: Long => Long = { (n: Long) =>
        timesCalculated += 1
        n match
          case 0 | 1 => n
          case n if n < 0 =>
            throw ArithmeticException()
          case _ => memoFib(n - 1) + memoFib(n - 2)
      }.memoized
      
      memoFib(n)
      
      // Operation must be O(n)
      timesCalculated mustEqual n + 1
    }
  }

  it must "memoize using the storage specified by the user" in {
    val poly = [X] => (x: X) => expensive(x)
    val f = poly.memoized(using scala.collection.mutable.WeakHashMap)
    val g = poly.memoized(using scala.collection.mutable.HashMap)

    assert(f.isInstanceOf[Storage.WeakHashMap])
    assert(g.isInstanceOf[Storage.HashMap])

    val mono = (x: Int) => expensive(x)
    val x = mono.memoized(using scala.collection.mutable.WeakHashMap)
    val y = mono.memoized(using scala.collection.mutable.HashMap)

    assert(x.isInstanceOf[Storage.WeakHashMap])
    assert(y.isInstanceOf[Storage.HashMap])

    // It is impossible to test context functions for this at the moment
  }

}
