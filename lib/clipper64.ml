open Conv

type t = C.Types.Clipper64.t Ctypes_static.ptr

let size = C.Funcs.clipper64_size () |> size_to_int
let destruct t = C.Funcs.destruct_clipper64 t

let alloc () =
  let finalise = Mem.finaliser C.Types.Clipper64.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.Clipper64.t) buf)

let make () =
  let buf, t = alloc () in
  let _ = C.Funcs.clipper64 buf in
  t

let add_subject t ps = C.Funcs.clipper64_add_subject t ps
let add_open_subject t ps = C.Funcs.clipper64_add_open_subject t ps
let add_clip t ps = C.Funcs.clipper64_add_clip t ps

let execute ?(fill_rule = `NonZero) ?open_solution ~op ~solution t =
  let op = ClipType.make op
  and fill_rule = FillRule.make fill_rule
  and open_sol =
    match open_solution with
    | Some open_solution -> open_solution
    | None -> Paths64.make ()
  in
  if C.Funcs.clipper64_execute t op fill_rule solution open_sol
  then Ok ()
  else Error "Clipper64 execution failed."

let execute_tree ?(fill_rule = `NonZero) ?open_solution ~op ~solution t =
  let op = ClipType.make op
  and fill_rule = FillRule.make fill_rule in
  if match open_solution with
     | Some open_sol ->
       C.Funcs.clipper64_execute_tree_with_open t op fill_rule solution open_sol
     | None -> C.Funcs.clipper64_execute_tree t op fill_rule solution
  then Ok ()
  else Error "Clipper64 execution failed."

let get_preserve_collinear t = C.Funcs.clipper64_get_preserve_collinear t
let set_preserve_collinear t pc = C.Funcs.clipper64_set_preserve_collinear t pc
let get_reverse_solution t = C.Funcs.clipper64_get_reverse_solution t
let set_reverse_solution t rs = C.Funcs.clipper64_set_reverse_solution t rs
let clear t = C.Funcs.clipper64_clear t
