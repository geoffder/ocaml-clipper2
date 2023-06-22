open Conv
open PathD_0

type t = C.Types.PathD.t Ctypes_static.ptr

let make () =
  let buf, t = alloc () in
  let _ = C.Funcs.pathd buf in
  t

let length t = size_to_int @@ C.Funcs.pathd_length t
let unsafe_get t i = C.Funcs.pathd_get_point t i

let get t i =
  if i >= 0 && i < length t
  then C.Funcs.pathd_get_point t i
  else invalid_arg "PathD.get: out of bounds access"

let reserve t n = C.Funcs.pathd_reserve t (size_of_int n)
let add_point t p = C.Funcs.pathd_add_point t p

let of_tups l =
  let t = make () in
  List.iter (fun p -> add_point t (PointD.of_tup p)) l;
  t

let ellipse ?(fn = 0) ?centre w h =
  let centre =
    match centre with
    | Some c -> c
    | None -> PointD.make 0. 0.
  and buf, p = alloc () in
  let _ = C.Funcs.pathd_ellipse buf centre (w *. 0.5) (h *. 0.5) fn in
  p

let translate t x y =
  let buf, translated = alloc () in
  let _ = C.Funcs.pathd_translate buf t x y in
  translated

let bounds t =
  let buf, rect = RectD_0.alloc () in
  let _ = C.Funcs.pathd_bounds buf t in
  rect

let rect_clip ?(precision = 2) ?(closed = true) t rect =
  let buf, paths = PathsD_0.alloc () in
  let _ =
    if closed
    then C.Funcs.pathd_rect_clip buf rect t precision
    else C.Funcs.pathd_rect_clip_line buf rect t precision
  in
  paths

let trim_collinear ?(precision = 2) ?(closed = true) t =
  let buf, trimmed = alloc () in
  let _ = C.Funcs.pathd_trim_collinear buf t closed precision in
  trimmed

let strip_near_equal ?(closed = true) ?(eps = 0.01) t =
  let buf, stripped = alloc () in
  let _ = C.Funcs.pathd_strip_near_equal buf t eps closed in
  stripped

let strip_duplicates ?(closed = true) t = C.Funcs.pathd_strip_duplicates t closed

let simplify ?(closed = true) ?(eps = 0.01) t =
  let buf, simplified = alloc () in
  let _ = C.Funcs.pathd_simplify buf t eps closed in
  simplified

let ramer_douglas_peucker ?(eps = 0.01) t =
  let buf, rmd = alloc () in
  let _ = C.Funcs.pathd_ramer_douglas_peucker buf t eps in
  rmd

let area t = C.Funcs.pathd_area t

let point_inside t p =
  match C.Funcs.pointd_in_path t p with
  | IsOutside -> `Outside
  | IsInside -> `Inside
  | IsOn -> `OnBorder

let is_positive t = C.Funcs.pathd_is_positive t

let minkowski_sum ?(precision = 2) ?(closed = true) ~pattern t =
  let buf, mink = PathsD_0.alloc () in
  let _ = C.Funcs.pathd_minkowski_sum buf pattern t closed precision in
  mink

let minkowski_diff ?(precision = 2) ?(closed = true) ~pattern t =
  let buf, mink = PathsD_0.alloc () in
  let _ = C.Funcs.pathd_minkowski_diff buf pattern t closed precision in
  mink
