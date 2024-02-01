import org.scalatest._
import flatspec._
import matchers._
import memoize.data.Eq
import memoize.Memo

class MemoizeSpec extends AnyFlatSpec with must.Matchers {

  private def benchmark[A](e: => A): (A, Long) =
    val start = System.currentTimeMillis()
    val result: A = e
    val end = System.currentTimeMillis()
    (result, end - start)

  """
  `memoize.apply`
  """ must "memoize a simple identity function" in {
    val f = memoize(identity[Int])
    f(1) mustEqual identity(1)
    f(1) mustNot equal(identity(2))
  }

  it must "memoize a curried function" in {
    val f = (a: Int) => (b: String, c: Char) => (a, b)
    val g = memoize(f)
    g(1)("foo", 'x') mustEqual f(1)("foo", 'x')
    g(1)("bar", 'y') mustNot equal(f(42)("foo", 'z'))
  }

  it must "allow comparison of arguments by a customaly supplied `Eq` instance" in {
    val f = memoize(identity[Int])

    given passEq[A]: Eq[A] with {
      def eqv(a: A, b: A): Boolean = true
    }

    f(using passEq)(1) mustEqual f(using passEq)(2)
  }

  it must "memoize implicit arguments the same way it does for normal arguments" in {
    val f = (x: Int) => (y: Int) ?=> x + y
    given Int = 5
    val g = memoize(f)
    f(3) mustEqual g(3).apply
  }

  it must "memoize a polymorphic function value if their type parameters are applied upon memoization" in {
    val poly1 = [X, Y] => (x: X, y: Y) => x == y
    val f = memoize(poly1[Int, Int])
    f(1, 1) mustEqual poly1(1, 1)
    
    val poly2 = [X] => (x: X) => [Y] => (y: Y) => x == y
    val g = memoize(poly2(_: Int)[Int])
    g(1)(1) mustEqual poly2(1)(1)
  }

  """
  `memoize.define`
  """ must "memoize a recursively defined factorial function" in {
    val factorial = memoize.define[Long, Long] { f => n =>
      n match
        case n if n < 0 =>
          throw ArithmeticException()
        case 0 | 1 => 1
        case _     => n * f(n - 1)
    }

    (15 * factorial(14)) mustEqual factorial(15)
  }

  it must "allow the use of other memoized functions inside their body definition" in {
    val memId = memoize(identity[Long])
    val factorial = memoize.define[Long, Long] { f => n =>
      n match
        case n if n <= 1 => 1
        case _           => n * memId(f(n - 1))
    }

    (15 * factorial(14)) mustEqual factorial(15)
  }

  it should "be significantly faster to compute the fibonacci sequence on large inputs" in {
    def fibb(n: BigInt): BigInt =
      n match
        case n if n < 0 =>
          throw ArithmeticException()
        case 0 => 0
        case 1 => 1
        case _ => fibb(n - 1) + fibb(n - 2)

    val memoFibb = memoize.define[BigInt, BigInt] { f => n =>
      n match
        case n if n < 0 =>
          throw ArithmeticException()
        case 0 => 0
        case 1 => 1
        case _ => f(n - 1) + f(n - 2)
    }

    (25 to 35) foreach { n =>
      val (r1, time1) = benchmark { fibb(n) }
      val (r2, time2) = benchmark { memoFibb(n) }
      assert(r1 == r2)
      assert(time2 < time1)
    }
  }
}
