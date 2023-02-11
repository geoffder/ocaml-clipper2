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

let length t = size_to_int @@ C.Funcs.path64_length t
