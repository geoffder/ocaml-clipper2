open Conv

(* NOTE:
If you are working with doubles, rather than int64, it is best to stick with
the InflatePaths API, as that is precision aware. The path adding methods
of the ClipperOffset class simply round off the doubles into integers, whereas
the double version of InflatePaths adheres to the desired precision by scaling
before and after casting. To bring this more in line, some double specific
specialization like with ClipperD would be needed (keeping track of
precision and applying it when adding paths / executing).

Will leave this here as is for now, but to avoid confusion, it will be excluded
from decimal interfaces. *)

type t = C.Types.Offset.t Ctypes_static.ptr

let size = C.Funcs.offset_size () |> size_to_int
let destruct t = C.Funcs.destruct_offset t

let alloc () =
  let finalise = Mem.finaliser C.Types.Offset.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.Offset.t) buf)

let make
  ?(miter_limit = 2.)
  ?(arc_tolerance = 0.)
  ?(preserve_collinear = false)
  ?(reverse_solution = false)
  ()
  =
  let buf, t = alloc () in
  let _ =
    C.Funcs.offset buf miter_limit arc_tolerance preserve_collinear reverse_solution
  in
  t

let add_pathd ?(join_type = `Round) ?(end_type = `Polygon) t p =
  let join_type = JoinType.make join_type
  and end_type = EndType.make end_type in
  C.Funcs.offset_add_pathd t p join_type end_type

let add_pathsd ?(join_type = `Round) ?(end_type = `Polygon) t p =
  let join_type = JoinType.make join_type
  and end_type = EndType.make end_type in
  C.Funcs.offset_add_pathsd t p join_type end_type

let add_path64 ?(join_type = `Round) ?(end_type = `Polygon) t p =
  let join_type = JoinType.make join_type
  and end_type = EndType.make end_type in
  C.Funcs.offset_add_path64 t p join_type end_type

let add_paths64 ?(join_type = `Round) ?(end_type = `Polygon) t p =
  let join_type = JoinType.make join_type
  and end_type = EndType.make end_type in
  C.Funcs.offset_add_paths64 t p join_type end_type

let execute ~delta t =
  let buf, paths = Paths64_0.alloc () in
  let _ = C.Funcs.offset_execute buf t delta in
  paths

let get_miter_limit t = C.Funcs.offset_get_miter_limit t
let set_miter_limit t lim = C.Funcs.offset_set_miter_limit t lim
let get_arc_tolerance t = C.Funcs.offset_get_arc_tolerance t
let set_arc_tolerance t tol = C.Funcs.offset_set_arc_tolerance t tol
let get_preserve_collinear t = C.Funcs.offset_get_preserve_collinear t
let set_preserve_collinear t pc = C.Funcs.offset_set_preserve_collinear t pc
let get_reverse_solution t = C.Funcs.offset_get_reverse_solution t
let set_reverse_solution t rs = C.Funcs.offset_set_reverse_solution t rs
let error_code t = C.Funcs.offset_error_code t
let clear t = C.Funcs.offset_clear t
