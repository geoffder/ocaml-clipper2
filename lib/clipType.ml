type t = C.Types.ClipType.t =
  | None
  | Intersection
  | Union
  | Difference
  | Xor

let make = function
  | `None -> None
  | `Intersection -> Intersection
  | `Union -> Union
  | `Difference -> Difference
  | `Xor -> Xor
