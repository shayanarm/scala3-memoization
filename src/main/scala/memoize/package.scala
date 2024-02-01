package object memoize {
  transparent inline def apply(inline e: Any): Any =
    ${ Memoize.memoize('e) }

  transparent inline def define[I, O](inline e: (I => O) => I => O): Any =
    ${ Memoize.memoizeRec[I, O]('e) }
}
