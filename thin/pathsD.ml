open Conv
include PathsD_0

let make () =
  let buf, t = alloc () in
  let _ = C.Funcs.pathsd buf in
  t

let add_path t p = C.Funcs.pathsd_add_path t p
let length t = size_to_int @@ C.Funcs.pathsd_length t
let path_length t i = size_to_int @@ C.Funcs.pathsd_path_length t i

let get_path t i =
  let buf, p = PathD.alloc () in
  let _ = C.Funcs.pathsd_get_path buf t i in
  p

let get_point t i j = C.Funcs.pathsd_get_point t i j

let translate t x y =
  let buf, translated = alloc () in
  let _ = C.Funcs.pathsd_translate buf t x y in
  translated

let boolean_op ?(fill_rule = `NonZero) ?(precision = 2) ~op subjects clips =
  let buf, t = alloc ()
  and op = ClipType.make op
  and fill_rule = FillRule.make fill_rule in
  let _ = C.Funcs.pathsd_boolean_op buf op fill_rule subjects clips precision in
  t

let boolean_op_tree ?(fill_rule = `NonZero) ?(precision = 2) ~op subjects clips =
  let tree = PolyTreeD.make ()
  and op = ClipType.make op
  and fill_rule = FillRule.make fill_rule in
  let _ = C.Funcs.pathsd_boolean_op_tree op fill_rule subjects clips tree precision in
  tree

let intersect ?fill_rule ?precision subjects clips =
  boolean_op ?fill_rule ?precision ~op:`Intersection subjects clips

let union ?fill_rule ?precision subjects =
  boolean_op ?fill_rule ?precision ~op:`Union subjects (make ())

let difference ?fill_rule ?precision subjects clips =
  boolean_op ?fill_rule ?precision ~op:`Difference subjects clips

let xor ?fill_rule ?precision subjects clips =
  boolean_op ?fill_rule ?precision ~op:`Xor subjects clips

let bounds t =
  let buf, rect = RectD_0.alloc () in
  let _ = C.Funcs.pathsd_bounds buf t in
  rect

let rect_clip ?(precision = 2) ?(closed = true) t rect =
  let buf, clipped = alloc () in
  let _ =
    if closed
    then C.Funcs.pathsd_rect_clip buf rect t precision
    else C.Funcs.pathsd_rect_clip_lines buf rect t precision
  in
  clipped

let inflate ?(precision = 2) ?(join_type = `Round) ?(end_type = `Polygon) ~delta t =
  let buf, inflated = alloc ()
  and join_type, miter_limit = JoinType.make join_type
  and end_type = EndType.make end_type in
  let _ = C.Funcs.pathsd_inflate buf t delta join_type end_type miter_limit precision in
  inflated

let strip_near_equal ?(closed = true) ?(eps = 0.01) t =
  let buf, stripped = alloc () in
  let _ = C.Funcs.pathsd_strip_near_equal buf t eps closed in
  stripped

let strip_duplicates ?(closed = true) t =
  let buf, stripped = alloc () in
  let _ = C.Funcs.pathsd_strip_duplicates buf t closed in
  stripped

let simplify ?(closed = true) ?(eps = 0.01) t =
  let buf, simplified = alloc () in
  let _ = C.Funcs.pathsd_simplify buf t eps closed in
  simplified

let ramer_douglas_peucker ?(eps = 0.01) t =
  let buf, rmd = alloc () in
  let _ = C.Funcs.pathsd_ramer_douglas_peucker buf t eps in
  rmd

let area t = C.Funcs.pathsd_area t

let minkowski_sum ?(precision = 2) ?(closed = true) ?(fill_rule = `NonZero) ~pattern t =
  let buf, mink = PathsD_0.alloc ()
  and fr = FillRule.make fill_rule in
  let _ = C.Funcs.pathsd_minkowski_sum buf pattern t closed precision fr in
  mink

let minkowski_diff ?(precision = 2) ?(closed = true) ?(fill_rule = `NonZero) ~pattern t =
  let buf, mink = PathsD_0.alloc ()
  and fr = FillRule.make fill_rule in
  let _ = C.Funcs.pathsd_minkowski_diff buf pattern t closed precision fr in
  mink
