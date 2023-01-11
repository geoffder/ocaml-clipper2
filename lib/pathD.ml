open Conv

type t = C.Types.PathD.t Ctypes_static.ptr

let size = C.Funcs.pathd_size () |> size_to_int
let destruct t = C.Funcs.destruct_pathd t

let alloc () =
  let finalise = Mem.finaliser C.Types.PathD.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.PathD.t) buf)

let make () =
  let buf, t = alloc () in
  let _ = C.Funcs.pathd buf in
  t

let length t = size_to_int @@ C.Funcs.pathd_length t

let of_string s =
  let buf, t = alloc () in
  let _ = C.Funcs.pathd_of_string buf (Conv.string_to_ptr Ctypes_static.char s) in
  t

let of_points ps =
  let buf, t = alloc ()
  and len = List.length ps in
  let pts = Ctypes.CArray.make C.Types.PointD.t len in
  List.iteri (fun i (x, y) -> Ctypes.CArray.set pts i (PointD.make x y)) ps;
  let _ = C.Funcs.pathd_of_points buf (Ctypes.CArray.start pts) (size_of_int len) in
  t

let ellipse ?(fn = 0) cx cy rx ry =
  let buf, t = alloc ()
  and centre = PointD.make cx cy in
  let _ = C.Funcs.pathd_ellipse buf centre rx ry fn in
  t

let add_point t x y = C.Funcs.pathd_add_point t (PointD.make x y)

let to_points t =
  let len = length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof C.Types.PointD.t)) in
  let pts = C.Funcs.pathd_to_points buf t in
  List.init len Ctypes.(fun i -> PointD.to_tup !@(pts +@ i))

let translate x y t =
  let buf, translated = alloc () in
  let _ = C.Funcs.pathd_translate buf t x y in
  translated

let trim_collinear ?(closed = true) ?(precision = 2) t =
  let buf, trimmed = alloc () in
  let _ = C.Funcs.pathd_trim_collinear buf t (not closed) precision in
  trimmed

let ramer_douglas_peucker ?(epsilon = 1e-6) t =
  let buf, rdp = alloc () in
  let _ = C.Funcs.pathd_ramer_douglas_peucker buf t epsilon in
  rdp
