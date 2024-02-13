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

  """
  `f.memoize`
  """ must "memoize a simple identity function with the type parameter applied" in {
    val f = (expensive[Int]).memoize
    val (r1, t1) = benchmark(f(1))
    val (r2, t2) = benchmark(f(1))
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize a curried function" in {
    val f: (Int => (String, Char) => (Int, String, Char)) =
      (((a: Int) => (b: String, c: Char) => expensive(a, b, c))).memoize
    val (r1, t1) = benchmark(f(42)("foo", 'c'))
    val (r2, t2) = benchmark(f(42)("foo", 'c'))
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize context functions the same way it does for normal functions" in {
    val f = (((x: Int) => (y: Int) ?=> expensive(x + y))).memoize
    given Int = 5
    val (r1: Int, t1) = benchmark(f(1).apply)
    val (r2: Int, t2) = benchmark(f(1).apply)
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize a Function0" in {
    val f = (() => expensive(42)).memoize
    val (r1, t1) = benchmark(f())
    val (r2, t2) = benchmark(f())
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize a monomorphic function with dependent arguments" in {
    val x = 42
    val f =
      (((a: x.type) => (b: Int) ?=> (c: b.type) => expensive((a, b)))).memoize
    val (r1, t1) = benchmark(f(x)(using 15)(15))
    val (r2, t2) = benchmark(f(x)(using 15)(15))
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize a polymorphic identity function" in {
    val f = ([X] => (x: X) => expensive(x)).memoize
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
    val f = ([X, Y] => (x: X, y: Y) => expensive((x, y))).memoize
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
      ([X, Y, Z] => (x: X) => (y: Y, z: Z) => expensive((x, y, z))).memoize
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
      .memoize(using HashMap)
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
      (([X] => (a: X) => (b: a.type) => expensive(a, b))).memoize
    val (r1, t1) = benchmark(f[Int](42)(42))
    val (r2, t2) = benchmark(f[Int](42)(42))
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize a polymorphic function with a type parameter bounded by another type parameter" in {
    case class Base()
    class Extended extends Base()
    val f = ([A, B <: A] => (a: A, b: B) => expensive((a, b))).memoize
    val (r1, t1) = benchmark(f(Base(), Extended()))
    val (r2, t2) = benchmark(f(Base(), Extended()))
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize nested polymorphic functions" in {
    val f = ([X] => (x: X) => [Y] => (y: Y) => expensive((x, y))).memoize
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
    ).memoize
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
    val f = ([X] => (x: X) ?=> expensive(x)).memoize
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
    }.memoize

    val (r1, t1) = benchmark(f(1))
    val (r2, t2) = benchmark(f(1))
    r1 mustEqual r2
    assert(t2 < t1)
  }

  it must "memoize a recursively defined polymorphic function lazy val" in {
    lazy val f: [X] => X => Boolean => X = { [X] => (n: X) => (b: Boolean) =>
        if b then f(n)(false) else expensive(n)
    }.memoize

    val (r1, t1) = benchmark(f(1)(false))
    val (r2, t2) = benchmark(f(1)(false))
    r1 mustEqual r2
    assert(t2 < t1)
  }  

  it must "make a recursively defined fibonacci lazy val be reliably faster on large inputs" in {

    def fibb(n: Long): Long =
      n match
        case 0 | 1 => n
        case n if n < 0 =>
          throw ArithmeticException()
        case _ => fibb(n - 1) + fibb(n - 2)

    lazy val memoFibb: Long => Long = { (n: Long) =>
      n match
        case 0 | 1 => n
        case n if n < 0 =>
          throw ArithmeticException()
        case _ => memoFibb(n - 1) + memoFibb(n - 2)
    }.memoize

    (35 to 40) foreach { n =>
      val (r1, t1) = benchmark { fibb(n) }
      val (r2, t2) = benchmark { memoFibb(n) }
      assert(r1 == r2)
      assert(t2 < t1)
    }
  }

  it must "properly memoize a recursively defined function lazy val on recursive invocations" in {

    def fibb(n: Long): Long =
      n match
        case 0 | 1 => n
        case n if n < 0 =>
          throw ArithmeticException()
        case _ => fibb(n - 1) + fibb(n - 2)
    
    val wrongFibb = fibb.memoize

    lazy val correctFibb: Long => Long = { (n: Long) =>
      n match
        case 0 | 1 => n
        case n if n < 0 =>
          throw ArithmeticException()
        case _ => correctFibb(n - 1) + correctFibb(n - 2)
    }.memoize

    (35 to 40) foreach { n =>
      val (r1, t1) = benchmark { wrongFibb(n) }
      val (r2, t2) = benchmark { correctFibb(n) }
      assert(r1 == r2)
      assert(t2 < t1)
    }
  }  
}
