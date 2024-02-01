package object memoize {
  transparent inline def apply(inline e: Any): Any =
    ${ Memoize.memoize('e) }

  inline def define[I, O](inline e: (I => O) => I => O): Memo[I, I => O] =
    ${ Memoize.memoizeRec[I, O]('e) }
}
