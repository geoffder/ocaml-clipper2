include RectD_0

let make ~l ~t ~r ~b =
  let buf, rect = alloc () in
  let _ = C.Funcs.rectd buf l t r b in
  rect

let width t = C.Funcs.rectd_width t
let height t = C.Funcs.rectd_height t
let midpoint t = C.Funcs.rectd_midpoint t

let scale s t =
  let buf, scaled = alloc () in
  let _ = C.Funcs.rectd_scale buf t s in
  scaled

let as_path t =
  let buf, p = PathD.alloc () in
  let _ = C.Funcs.rectd_as_path buf t in
  p

let contains_pt t p = C.Funcs.rectd_contains_pt t p
let contains_rect a b = C.Funcs.rectd_contains_rect a b
let is_empty t = C.Funcs.rectd_is_empty t
let intersects a b = C.Funcs.rectd_intersects a b
