type t = C.Types.EndType.t =
  | Polygon
  | Joined
  | Butt
  | Square
  | Round

let to_string = function
  | Polygon -> "POLYGON_END"
  | Joined -> "JOINED_END"
  | Butt -> "BUTT_END"
  | Square -> "SQUARE_END"
  | Round -> "ROUND_END"
