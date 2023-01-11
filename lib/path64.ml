open Conv

type t = C.Types.Path64.t Ctypes_static.ptr

let size = C.Funcs.path64_size () |> size_to_int
let destruct t = C.Funcs.destruct_path64 t

let alloc () =
  let finalise = Mem.finaliser C.Types.Path64.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.Path64.t) buf)

let make () =
  let buf, t = alloc () in
  let _ = C.Funcs.path64 buf in
  t

let of_string s =
  let buf, t = alloc () in
  let _ = C.Funcs.path64_of_string buf (Conv.string_to_ptr Ctypes_static.char s) in
  t

let ellipse ?(fn = 0) cx cy rx ry =
  let buf, t = alloc () in
  let centre = Point64.make cx cy in
  let _ = C.Funcs.path64_ellipse buf centre rx ry fn in
  t

let add_point t x y = C.Funcs.path64_add_point t (Point64.make x y)

let translate x y t =
  let buf, translated = alloc () in
  let _ = C.Funcs.path64_translate buf t x y in
  translated

let length t = size_to_int @@ C.Funcs.path64_length t

let to_points t =
  let len = length t in
  let buf = Mem.allocate_buf (len * Ctypes.(sizeof C.Types.Point64.t)) in
  let pts = C.Funcs.path64_to_points buf t in
  List.init len Ctypes.(fun i -> Point64.to_tup !@(pts +@ i))
