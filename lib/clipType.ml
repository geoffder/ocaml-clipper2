type t = C.Types.ClipType.t =
  | None
  | Intersection
  | Union
  | Difference
  | Xor

let to_string = function
  | None -> "NONE"
  | Intersection -> "INTERSECTION"
  | Union -> "UNION"
  | Difference -> "DIFFERENCE"
  | Xor -> "XOR"
