open RectD_0

type t = RectD_0.t

let make ~l ~t ~r ~b =
  let buf, rect = alloc () in
  let _ = C.Funcs.rectd buf l t r b in
  rect

let of_pts a b =
  let buf, t = alloc ()
  and left = Float.min (PointD.x a) (PointD.x b)
  and right = Float.max (PointD.x a) (PointD.x b)
  and bottom = Float.max (PointD.y a) (PointD.y b)
  and top = Float.min (PointD.y a) (PointD.y b) in
  let _ = C.Funcs.rectd buf left top right bottom in
  t

let width t = C.Funcs.rectd_width t
let height t = C.Funcs.rectd_height t
let midpoint t = C.Funcs.rectd_midpoint t

let scale s t =
  let buf, scaled = alloc () in
  let _ = C.Funcs.rectd_scale buf t s in
  scaled

let as_path t =
  let buf, p = PathD_0.alloc () in
  let _ = C.Funcs.rectd_as_path buf t in
  p

let contains_pt t p = C.Funcs.rectd_contains_pt t p
let contains_rect a b = C.Funcs.rectd_contains_rect a b
let is_empty t = C.Funcs.rectd_is_empty t
let intersects a b = C.Funcs.rectd_intersects a b
