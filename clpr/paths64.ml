open Conv
include Paths64_0

let make () =
  let buf, t = alloc () in
  let _ = C.Funcs.paths64 buf in
  t

let reserve t n = C.Funcs.paths64_reserve t (size_of_int n)
let add_path t p = C.Funcs.paths64_add_path t p

let of_tups l =
  let t = make () in
  List.iter (fun s -> add_path t (Path64.of_tups s)) l;
  t

let of_path64 p =
  let t = make () in
  add_path t p;
  t

let length t = size_to_int @@ C.Funcs.paths64_length t
let unsafe_sublength t i = size_to_int @@ C.Funcs.paths64_path_length t i

let sublength t i =
  if i < 0 && i > length t
  then unsafe_sublength t i
  else invalid_arg "Paths64.sublength: out of bounds access"

let unsafe_subpath t i =
  let buf, p = Path64_0.alloc () in
  let _ = C.Funcs.paths64_get_path buf t i in
  p

let subpath t i =
  if i < 0 && i > length t
  then unsafe_subpath t i
  else invalid_arg "Paths64.subpath: out of bounds access"

let unsafe_get t i j = C.Funcs.paths64_get_point t i j

let get t i j =
  if i >= 0 && i < length t && j >= 0 && j < unsafe_sublength t i
  then unsafe_get t i j
  else invalid_arg "Paths64.get: out of bounds access"

let translate t x y =
  let buf, translated = alloc () in
  let _ = C.Funcs.paths64_translate buf t x y in
  translated

let boolean_op ?(fill_rule = `NonZero) ~op subjects clips =
  let buf, t = alloc ()
  and op = ClipType.make op
  and fill_rule = FillRule.make fill_rule in
  let _ = C.Funcs.paths64_boolean_op buf op fill_rule subjects clips in
  t

let boolean_op_tree ?(fill_rule = `NonZero) ~op subjects clips =
  let tree = PolyTree64.make ()
  and op = ClipType.make op
  and fill_rule = FillRule.make fill_rule in
  let _ = C.Funcs.paths64_boolean_op_tree op fill_rule subjects clips tree in
  tree

let intersect ?fill_rule subjects clips =
  boolean_op ?fill_rule ~op:`Intersection subjects clips

let union ?fill_rule subjects = boolean_op ?fill_rule ~op:`Union subjects (make ())

let difference ?fill_rule subjects clips =
  boolean_op ?fill_rule ~op:`Difference subjects clips

let xor ?fill_rule subjects clips = boolean_op ?fill_rule ~op:`Xor subjects clips

let bounds t =
  let buf, rect = Rect64_0.alloc () in
  let _ = C.Funcs.paths64_bounds buf t in
  rect

let rect_clip ?(closed = true) t rect =
  let buf, clipped = alloc () in
  let _ =
    if closed
    then C.Funcs.paths64_rect_clip buf rect t
    else C.Funcs.paths64_rect_clip_lines buf rect t
  in
  clipped

let inflate ?(miter_limit = 2.) ?(join_type = `Round) ?(end_type = `Polygon) ~delta t =
  if miter_limit < 2. then invalid_arg "Miter limit can be no less than 2.";
  let buf, inflated = alloc ()
  and join_type = JoinType.make join_type
  and end_type = EndType.make end_type in
  let _ = C.Funcs.paths64_inflate buf t delta join_type end_type miter_limit in
  inflated

let strip_near_equal ?(closed = true) ?(eps = 0.01) t =
  let buf, stripped = alloc () in
  let _ = C.Funcs.paths64_strip_near_equal buf t eps closed in
  stripped

let strip_duplicates ?(closed = true) t = C.Funcs.paths64_strip_duplicates t closed

let simplify ?(closed = true) ?(eps = 0.01) t =
  let buf, simplified = alloc () in
  let _ = C.Funcs.paths64_simplify buf t eps closed in
  simplified

let ramer_douglas_peucker ?(eps = 0.01) t =
  let buf, rmd = alloc () in
  let _ = C.Funcs.paths64_ramer_douglas_peucker buf t eps in
  rmd

let area t = C.Funcs.paths64_area t

let minkowski_sum ?(closed = true) ?(fill_rule = `NonZero) ~pattern t =
  let buf, mink = Paths64_0.alloc ()
  and fr = FillRule.make fill_rule in
  let _ = C.Funcs.paths64_minkowski_sum buf pattern t closed fr in
  mink

let minkowski_diff ?(closed = true) ?(fill_rule = `NonZero) ~pattern t =
  let buf, mink = Paths64_0.alloc ()
  and fr = FillRule.make fill_rule in
  let _ = C.Funcs.paths64_minkowski_diff buf pattern t closed fr in
  mink
