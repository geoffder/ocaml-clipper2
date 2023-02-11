open Conv

type t = C.Types.ClipperD.t Ctypes_static.ptr

let size = C.Funcs.clipperd_size () |> size_to_int
let destruct t = C.Funcs.destruct_clipperd t

let alloc () =
  let finalise = Mem.finaliser C.Types.ClipperD.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.ClipperD.t) buf)

let make ?(precision = 2) () =
  let buf, t = alloc () in
  let _ = C.Funcs.clipperd buf precision in
  t

let add_subject t ps = C.Funcs.clipperd_add_subject t ps
let add_open_subject t ps = C.Funcs.clipperd_add_open_subject t ps
let add_clip t ps = C.Funcs.clipperd_add_clip t ps

let execute ?(fill_rule = `NonZero) ?open_solution ~op ~solution t =
  let op = ClipType.make op
  and fill_rule = FillRule.make fill_rule
  and open_sol =
    match open_solution with
    | Some open_solution -> open_solution
    | None -> PathsD.make ()
  in
  if C.Funcs.clipperd_execute t op fill_rule solution open_sol
  then Ok ()
  else Error "ClipperD execution failed."

let execute_tree ?(fill_rule = `NonZero) ?open_solution ~op ~solution t =
  let op = ClipType.make op
  and fill_rule = FillRule.make fill_rule in
  if match open_solution with
     | Some open_sol ->
       C.Funcs.clipperd_execute_tree_with_open t op fill_rule solution open_sol
     | None -> C.Funcs.clipperd_execute_tree t op fill_rule solution
  then Ok ()
  else Error "ClipperD execution failed."

let get_preserve_collinear t = C.Funcs.clipperd_get_preserve_collinear t
let set_preserve_collinear t pc = C.Funcs.clipperd_set_preserve_collinear t pc
let get_reverse_solution t = C.Funcs.clipperd_get_reverse_solution t
let set_reverse_solution t rs = C.Funcs.clipperd_set_reverse_solution t rs
let clear t = C.Funcs.clipperd_clear t
