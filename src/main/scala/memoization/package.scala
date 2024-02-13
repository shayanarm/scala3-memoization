import memoization.quoted.Macros

package object memoization {

  extension [F](f: F)
    @scala.annotation.experimental
    transparent inline def memoize[S](using strategy: S = scala.collection.mutable.WeakHashMap) = ${ Macros.memoizeImpl[S]('f) }
}
