include Rect64_0

let make ~l ~t ~r ~b =
  let buf, rect = alloc () in
  let _ = C.Funcs.rect64 buf l t r b in
  rect

let of_pts a b =
  let buf, t = alloc ()
  and left = Int64.min (Point64.x a) (Point64.x b)
  and right = Int64.max (Point64.x a) (Point64.x b)
  and bottom = Int64.max (Point64.y a) (Point64.y b)
  and top = Int64.min (Point64.y a) (Point64.y b) in
  let _ = C.Funcs.rect64 buf left top right bottom in
  t

let width t = C.Funcs.rect64_width t
let height t = C.Funcs.rect64_height t
let midpoint t = C.Funcs.rect64_midpoint t

let scale s t =
  let buf, scaled = alloc () in
  let _ = C.Funcs.rect64_scale buf t s in
  scaled

let as_path t =
  let buf, p = Path64_0.alloc () in
  let _ = C.Funcs.rect64_as_path buf t in
  p

let contains_pt t p = C.Funcs.rect64_contains_pt t p
let contains_rect a b = C.Funcs.rect64_contains_rect a b
let is_empty t = C.Funcs.rect64_is_empty t
let intersects a b = C.Funcs.rect64_intersects a b
