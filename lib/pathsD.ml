open Ctypes
open Conv

type t = C.Types.PathsD.t Ctypes_static.ptr

let size = C.Funcs.pathsd_size () |> size_to_int
let destruct t = C.Funcs.destruct_pathsd t

let alloc () =
  let finalise = Mem.finaliser C.Types.PathsD.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(coerce (ptr void) (ptr C.Types.PathsD.t) buf)

let make () =
  let buf, t = alloc () in
  let _ = C.Funcs.pathsd buf in
  t

let add_path t p = C.Funcs.pathsd_add_path t p
let length t = size_to_int @@ C.Funcs.pathsd_length t
let path_length t i = size_to_int @@ C.Funcs.pathsd_path_length t i

let get_path t i =
  let buf, p = PathD.alloc () in
  let _ = C.Funcs.pathsd_get_path buf t i in
  p
