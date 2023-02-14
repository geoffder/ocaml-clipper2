open Conv

type t = C.Types.Rect64.t Ctypes_static.ptr

let size = C.Funcs.rect64_size () |> size_to_int
let destruct t = C.Funcs.destruct_rect64 t

let alloc () =
  let finalise = Mem.finaliser C.Types.Rect64.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.Rect64.t) buf)
