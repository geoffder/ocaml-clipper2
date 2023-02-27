type color =
  | Black
  | White
  | Blue
  | Gray
  | Green
  | Purple
  | Red
  | Lime
  | Yellow
  | Cyan
  | Magenta
  | Silver
  | Teal
  | Navy
  | Hex of int
  | RGB of int * int * int

type t =
  { color : color
  ; alpha : float
  }

let make ?(alpha = 0.8) color =
  { color; alpha = (if alpha < 0. then 0. else if alpha > 1. then 1. else alpha) }

let to_int { color; alpha } =
  let a = Int.(of_float (alpha *. to_float 0xFF000000))
  and c =
    match color with
    | Black -> 0x000000
    | White -> 0xFFFFFF
    | Blue -> 0x0000FF
    | Gray -> 0x808080
    | Green -> 0x008000
    | Purple -> 0x800080
    | Red -> 0xFF0000
    | Lime -> 0x00FF00
    | Yellow -> 0xFFFF00
    | Cyan -> 0x00FFFF
    | Magenta -> 0xFF00FF
    | Silver -> 0xC0C0C0
    | Teal -> 0x008080
    | Navy -> 0x000080
    | Hex c -> c
    | RGB (r, g, b) -> (256 * 256 * r) + (256 * g) + b
  in
  a + c
