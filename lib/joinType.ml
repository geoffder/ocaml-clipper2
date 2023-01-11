type t = C.Types.JoinType.t =
  | Square
  | Round
  | Miter

let to_string = function
  | Square -> "SQUARE_JOIN"
  | Round -> "ROUND_JOIN"
  | Miter -> "MITER_JOIN"
