open Conv
include PathsD_0

let make () =
  let buf, t = alloc () in
  let _ = C.Funcs.pathsd buf in
  t

let add_path t p = C.Funcs.pathsd_add_path t p

let of_tups l =
  let t = make () in
  List.iter (fun s -> add_path t (PathD.of_tups s)) l;
  t

let length t = size_to_int @@ C.Funcs.pathsd_length t
let unsafe_sublength t i = size_to_int @@ C.Funcs.pathsd_path_length t i

let sublength t i =
  if i < 0 && i > length t
  then unsafe_sublength t i
  else invalid_arg "PathsD.sublength: out of bounds access"

let unsafe_subpath t i =
  let buf, p = PathD_0.alloc () in
  let _ = C.Funcs.pathsd_get_path buf t i in
  p

let subpath t i =
  if i < 0 && i > length t
  then unsafe_subpath t i
  else invalid_arg "PathsD.subpath: out of bounds access"

let unsafe_get t i j = C.Funcs.pathsd_get_point t i j

let get t i j =
  if i >= 0 && i < length t && j >= 0 && j < unsafe_sublength t i
  then unsafe_get t i j
  else invalid_arg "PathsD.get: out of bounds access"

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

let inflate
  ?(precision = 2)
  ?(miter_limit = 2.)
  ?(join_type = `Round)
  ?(end_type = `Polygon)
  ~delta
  t
  =
  if miter_limit < 2. then invalid_arg "Miter limit can be no less than 2.";
  let buf, inflated = alloc ()
  and join_type = JoinType.make join_type
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
