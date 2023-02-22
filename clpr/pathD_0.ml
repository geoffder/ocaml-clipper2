open Conv
type t = C.Types.PathD.t Ctypes_static.ptr

let size = C.Funcs.pathd_size () |> size_to_int
let destruct t = C.Funcs.destruct_pathd t

let alloc () =
  let finalise = Mem.finaliser C.Types.PathD.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.PathD.t) buf)
