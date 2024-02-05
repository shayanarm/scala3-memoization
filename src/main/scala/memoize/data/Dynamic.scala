package memoize.data

import scala.reflect.Typeable

class Dynamic private(value: Any, proof: Typeable[_]) {
    def safeCast[T: Typeable]: Option[T] =
        value match
            case proof(t: T) => Some(t)
}

object Dynamic:
    def apply[T: Typeable](v: T): Dynamic = Dynamic(v, summon[Typeable[T]])
