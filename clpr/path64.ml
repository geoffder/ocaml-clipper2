open Conv
include Path64_0

let make () =
  let buf, t = alloc () in
  let _ = C.Funcs.path64 buf in
  t

let length t = size_to_int @@ C.Funcs.path64_length t
let unsafe_get t i = C.Funcs.path64_get_point t i

let get t i =
  if i >= 0 && i < length t
  then C.Funcs.path64_get_point t i
  else invalid_arg "Path64.get: out of bounds access"

let add_point t p = C.Funcs.path64_add_point t p

let of_tups l =
  let t = make () in
  List.iter (fun p -> add_point t (Point64.of_tup p)) l;
  t

let ellipse ?(fn = 0) ?centre w h =
  let centre =
    match centre with
    | Some c -> c
    | None -> Point64.make (Int64.of_int 0) (Int64.of_int 0)
  and buf, t = alloc ()
  and rx = (Int64.to_float @@ w) *. 0.5
  and ry = (Int64.to_float @@ h) *. 0.5 in
  let _ = C.Funcs.path64_ellipse buf centre rx ry fn in
  t

let translate t x y =
  let buf, translated = alloc () in
  let _ = C.Funcs.path64_translate buf t x y in
  translated

let bounds t =
  let buf, rect = Rect64_0.alloc () in
  let _ = C.Funcs.path64_bounds buf t in
  rect

let rect_clip ?(closed = true) t rect =
  let buf, paths = Paths64_0.alloc () in
  let _ =
    if closed
    then C.Funcs.path64_rect_clip buf rect t
    else C.Funcs.path64_rect_clip_line buf rect t
  in
  paths

let trim_collinear ?(closed = true) t =
  let buf, trimmed = alloc () in
  let _ = C.Funcs.path64_trim_collinear buf t closed in
  trimmed

let strip_near_equal ?(closed = true) ?(eps = 1.) t =
  let buf, stripped = alloc () in
  let _ = C.Funcs.path64_strip_near_equal buf t eps closed in
  stripped

let strip_duplicates ?(closed = true) t =
  let buf, stripped = alloc () in
  let _ = C.Funcs.path64_strip_duplicates buf t closed in
  stripped

let simplify ?(closed = true) ?(eps = 1.) t =
  let buf, simplified = alloc () in
  let _ = C.Funcs.path64_simplify buf t eps closed in
  simplified

let ramer_douglas_peucker ?(eps = 1.) t =
  let buf, rmd = alloc () in
  let _ = C.Funcs.path64_ramer_douglas_peucker buf t eps in
  rmd

let area t = C.Funcs.path64_area t

let point_inside t p =
  match C.Funcs.point64_in_path t p with
  | IsOutside -> `Outside
  | IsInside -> `Inside
  | IsOn -> `OnBorder

let is_positive t = C.Funcs.path64_is_positive t

let minkowski_sum ?(closed = true) ~pattern t =
  let buf, mink = Paths64_0.alloc () in
  let _ = C.Funcs.path64_minkowski_sum buf pattern t closed in
  mink

let minkowski_diff ?(closed = true) ~pattern t =
  let buf, mink = Paths64_0.alloc () in
  let _ = C.Funcs.path64_minkowski_diff buf pattern t closed in
  mink
