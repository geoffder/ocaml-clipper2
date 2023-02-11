open Ctypes
open Conv

type t = C.Types.Paths64.t Ctypes_static.ptr

let size = C.Funcs.paths64_size () |> size_to_int
let destruct t = C.Funcs.destruct_paths64 t

let alloc () =
  let finalise = Mem.finaliser C.Types.Paths64.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(coerce (ptr void) (ptr C.Types.Paths64.t) buf)

let make () =
  let buf, t = alloc () in
  let _ = C.Funcs.paths64 buf in
  t

let add_path t p = C.Funcs.paths64_add_path t p
let length t = size_to_int @@ C.Funcs.paths64_length t
let path_length t i = size_to_int @@ C.Funcs.paths64_path_length t i

let get_path t i =
  let buf, p = Path64.alloc () in
  let _ = C.Funcs.paths64_get_path buf t i in
  p
