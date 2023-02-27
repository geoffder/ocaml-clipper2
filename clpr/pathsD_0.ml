open Ctypes
open Conv

type t = C.Types.PathsD.t Ctypes_static.ptr

let size = C.Funcs.pathsd_size () |> size_to_int
let destruct t = C.Funcs.destruct_pathsd t

let alloc () =
  let finalise = Mem.finaliser C.Types.PathsD.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(coerce (ptr void) (ptr C.Types.PathsD.t) buf)
