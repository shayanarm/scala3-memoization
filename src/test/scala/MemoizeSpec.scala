import org.scalatest._
import flatspec._
import matchers._
import memoize.data.Eq
import memoize.Memo

class MemoizeSpec extends AnyFlatSpec with must.Matchers {
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
    
    given twistedEq[A]: Eq[A] with {
        def eqv(a: A, b: A): Boolean = true
    } 

    f(using twistedEq)(1) mustEqual f(using twistedEq)(2)
  }
  
  it must "memoize implicit arguments the same way it treats normal arguments" in {
    val f = (x: Int) => (y: Int) ?=> x + y
    given Int = 5
    val g = memoize(f)
    (f(3): Int) mustEqual g(3).apply
  }

  """
  `memoize.define`
  """ must "memoize a recursively defined factorial function" in {
    val factorial = memoize.define[Long, Long] { f => n =>
      n match
        case n if n <= 1 => 1
        case _           => n * f(n - 1)
    }

    (15 * factorial(14)) mustEqual factorial(15)
  }
  
  it must "allow the use of other memoized functions inside their definition body" in {
    val memId = memoize(identity[Long])
    val factorial = memoize.define[Long, Long] { f => n =>
      n match
        case n if n <= 1 => 1
        case _           => n * memId(f(n - 1))
    }

    (15 * factorial(14)) mustEqual factorial(15)
  }
}
