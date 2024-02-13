package com.github.shayanarm

import memoization.quoted.Macros

package object memoization {

  extension [F](f: F)
    @scala.annotation.experimental
    transparent inline def memoized[S](using storage: S = scala.collection.mutable.WeakHashMap) = ${ Macros.memoizeImpl[S]('f) }
}
