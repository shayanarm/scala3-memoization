# Memoization for Scala 3
 
This small *Scala 3* library provides macro-generated memoization for functions of all arities, signatures, and even polymorphic function values. Your memoized function signature is guaranteed to remain identical to the original.

***Note:*** The library is marked ```@experimental``` as the macro features used to write it are also ```@experimental```. I will remove the requirement once Scala eventually stabilizes the `quoted.*` API.
```scala
def expensive[A](a: => A): A =
  Thread.sleep(1000)
  a

// There is only one extension method in the package: 

import com.github.shayanarm.memoization.memoized

val efficientInt = expensive[Int].memoized

efficientInt(5) // The first call will compute  
efficientInt(5) // The second call will not 

val efficientString = expensive[String].memoized

efficientString("foo") // The first call will compute  
efficientString("foo") // The second call will not
```

But, there is also a way to memoize polymorphic functions without creating separate instances for each parameter set unlike above:
```scala
val efficientAll: [X] => (a: X) => X = ([X] => (a: X) => expensive[X](a)).memoized
efficientAll(5) // This call will compute,
efficientAll("foo") // so does this,
efficientAll(5) // but neither this,
efficientAll("foo") // nor this
```
Your function signature will remain untouched. Context arguments are treated as normal arguments and are still memoized:
```scala
val f: Int => Int ?=> Int = (((x: Int) => (y: Int) ?=> expensive(x + y))).memoized
f(42)(using 21)
```
* You may partially apply your function before calling `.memoized` to exclude any parameter from being memoized.
* Curried functions are also recursively memorized.

What if you need to memoize a recursive function definition? Then, use the lazy val technique to define your memoized function:
```scala
lazy val fib: Long => Long = { (n: Long) =>
  n match
    case 0 | 1 => n
    case n if n < 0 =>
      throw ArithmeticException()
    case _ => fib(n - 1) + fib(n - 2)
}.memoized
// This will be O(n). Your original function will run only 36 times. 
fib(35)
// The unmemoized version runs 29860703 times on n = 35
// --- Caution ---
// Do not memoize post-definition as it will not memoize the recursive
// invocations inside the function body:
// def fib(n: Long): Long =
//   n match
//     case 0 | 1 => n
//     case n if n < 0 =>
//       throw ArithmeticException()
//     case _ => fib(n - 1) + fib(n - 2)

// val badFib = fib.memoized
```
The memoization is backed by two choices of data structures: 
* ```scala.collections.mutable.WeakHashMap``` 
* ```scala.collections.mutable.HashMap```

The default choice is ```WeakHashMap``` as it reduces the risk of memory leaks. You may, however, change that by:
```scala
f.memoized(using scala.collections.mutable.HashMap)
f.memoized(using scala.collections.mutable.WeakHashMap) // Already the default if unspecified
```
The library internally updates and reads the hashmaps in a thread-safe manner.
