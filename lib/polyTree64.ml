open Conv

type t = C.Types.PolyTree64.t Ctypes_static.ptr

let size = C.Funcs.polytree64_size () |> size_to_int
let destruct t = C.Funcs.destruct_polytree64 t

let alloc () =
  let finalise = Mem.finaliser C.Types.PolyTree64.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.PolyTree64.t) buf)

let make ?parent () =
  let parent =
    match parent with
    | Some t -> t
    | None -> Ctypes.(coerce (ptr void) (ptr C.Types.PolyTree64.t) null)
  and buf, t = alloc () in
  let _ = C.Funcs.polytree64 buf parent in
  t

let clear t = C.Funcs.polytree64_clear t
let add_child t path = C.Funcs.polytree64_add_child t path

let parent t =
  let p = C.Funcs.polytree64_parent t in
  if Ctypes.is_null p then None else Some p

let child t i = C.Funcs.polytree64_get_child t (size_of_int i)
let level t = C.Funcs.polytree64_level t
let count t = size_to_int @@ C.Funcs.polytree64_count t
let is_hole t = C.Funcs.polytree64_is_hole t
let area t = C.Funcs.polytree64_area t

let polygon t =
  let buf, path = Path64.alloc () in
  let _ = C.Funcs.polytree64_polygon buf t in
  path

let decompose to_vs t =
  let polys = ref Seq.empty in
  let rec outer i t =
    let n_outlines = count t in
    if i < n_outlines
    then (
      let outline = child t i in
      let holes = inner outline (count outline) 0 Seq.empty in
      polys := Seq.cons (Seq.cons (to_vs (polygon outline)) holes) !polys;
      if i < n_outlines - 1 then outer (i + 1) t else () )
  and inner outline n_holes j holes =
    if j < n_holes
    then (
      let c = child outline j in
      outer 0 c;
      inner outline n_holes (j + 1) (Seq.cons (to_vs (polygon c)) holes) )
    else holes
  in
  outer 0 t;
  !polys
