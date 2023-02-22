type t = C.Types.EndType.t =
  | Polygon
  | Joined
  | Butt
  | Square
  | Round

let make = function
  | `Polygon -> Polygon
  | `Joined -> Joined
  | `Butt -> Butt
  | `Square -> Square
  | `Round -> Round
