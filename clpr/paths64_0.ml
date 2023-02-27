open Ctypes
open Conv

type t = C.Types.Paths64.t Ctypes_static.ptr

let size = C.Funcs.paths64_size () |> size_to_int
let destruct t = C.Funcs.destruct_paths64 t

let alloc () =
  let finalise = Mem.finaliser C.Types.Paths64.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(coerce (ptr void) (ptr C.Types.Paths64.t) buf)
