open Conv

type t = C.Types.PolyTreeD.t Ctypes_static.ptr

let size = C.Funcs.polytreed_size () |> size_to_int
let destruct t = C.Funcs.destruct_polytreed t

let alloc () =
  let finalise = Mem.finaliser C.Types.PolyTreeD.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.PolyTreeD.t) buf)

let make ?parent () =
  let parent =
    match parent with
    | Some t -> t
    | None -> Ctypes.(coerce (ptr void) (ptr C.Types.PolyTreeD.t) null)
  and buf, t = alloc () in
  let _ = C.Funcs.polytreed buf parent in
  t

let clear t = C.Funcs.polytreed_clear t
let set_inv_scale t s = C.Funcs.polytreed_set_inv_scale t s
let inv_scale t = C.Funcs.polytreed_inv_scale t
let add_child t path = C.Funcs.polytreed_add_child t path

let parent t =
  let p = C.Funcs.polytreed_parent t in
  if Ctypes.is_null p then None else Some p

let child t i = C.Funcs.polytreed_get_child t (size_of_int i)
let level t = C.Funcs.polytreed_level t
let count t = size_to_int @@ C.Funcs.polytreed_count t
let is_hole t = C.Funcs.polytreed_is_hole t
let area t = C.Funcs.polytreed_area t

let polygon t =
  let buf, path = PathD.alloc () in
  let _ = C.Funcs.polytreed_polygon buf t in
  path

(* http://www.angusj.com/clipper2/Docs/Units/Clipper.Engine/Classes/PolyTreeD/_Body.htm
   A PolyTreeD object is a container for any number of PolyPathD child objects,
   each representing a single polygon contour. PolyTreeD's top level children
   will always be outer polygon contours. PolyPathD children may in turn contain
   their own children to any level of nesting. Children of outer polygon
   contours will always represent holes, and children of holes will always
   represent nested outer polygon contours.

   This function converts the above described tree structure into a list of
   complex polygons (outlines with holes). *)
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
