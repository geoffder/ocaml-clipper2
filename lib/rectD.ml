open Conv

type t = C.Types.RectD.t Ctypes_static.ptr

let size = C.Funcs.rectd_size () |> size_to_int
let destruct t = C.Funcs.destruct_rectd t

let alloc () =
  let finalise = Mem.finaliser C.Types.RectD.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.RectD.t) buf)
