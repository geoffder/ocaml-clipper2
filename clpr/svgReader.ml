open Conv

type t = C.Types.SvgReader.t Ctypes_static.ptr

let size = C.Funcs.svgreader_size () |> size_to_int
let destruct t = C.Funcs.destruct_svgreader t

let alloc () =
  let finalise = Mem.finaliser C.Types.SvgReader.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.SvgReader.t) buf)

let make () =
  let buf, t = alloc () in
  let _ = C.Funcs.svgreader buf in
  t

let load t file = C.Funcs.svgreader_load_from_file t (string_to_ptr Ctypes.char file)

let get_pathsd t =
  let buf, paths = PathsD_0.alloc () in
  let _ = C.Funcs.svgreader_get_pathsd buf t in
  paths

let clear t = C.Funcs.svgreader_clear t
