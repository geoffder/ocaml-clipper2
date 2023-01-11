type t = C.Types.FillRule.t =
  | EvenOdd
  | NonZero
  | Positive
  | Negative

let to_string = function
  | EvenOdd -> "EVEN_ODD"
  | NonZero -> "NON_ZERO"
  | Positive -> "POSITIVE"
  | Negative -> "NEGATIVE"
