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

let translate x y t =
  let buf, translated = alloc () in
  let _ = C.Funcs.pathsd_translate buf t x y in
  translated

let length t = size_to_int @@ C.Funcs.pathsd_length t

let get t i =
  let buf, p = PathD.alloc () in
  let _ = C.Funcs.pathsd_get buf t i in
  p

let to_points t =
  let n = length t in
  let lens = CArray.make size_t n in
  let _ = C.Funcs.pathsd_lengths (to_voidp @@ CArray.start lens) t in
  let bufs = CArray.make (ptr void) n in
  for i = 0 to n - 1 do
    let len = size_to_int @@ CArray.get lens i in
    let buf = CArray.make C.Types.Point64.t len in
    CArray.set bufs i (to_voidp @@ CArray.start buf)
  done;
  let pts = C.Funcs.pathsd_to_points (CArray.start bufs) t in
  List.init n (fun i ->
      let ps = !@(pts +@ i) in
      List.init (size_to_int (CArray.get lens i)) (fun j -> PointD.to_tup !@(ps +@ j)) )

let intersect ?(fill_rule = FillRule.NonZero) ?(precision = 2) subjects clips =
  let buf, t = alloc () in
  let _ = C.Funcs.pathsd_intersect buf subjects clips fill_rule precision in
  t

let union ?(fill_rule = FillRule.NonZero) ?(precision = 2) subjects clips =
  let buf, t = alloc () in
  let _ = C.Funcs.pathsd_union buf subjects clips fill_rule precision in
  t

let difference ?(fill_rule = FillRule.NonZero) ?(precision = 2) subjects clips =
  let buf, t = alloc () in
  let _ = C.Funcs.pathsd_difference buf subjects clips fill_rule precision in
  t

let xor ?(fill = FillRule.NonZero) ?(precision = 2) subjects clips =
  let buf, t = alloc () in
  let _ = C.Funcs.pathsd_xor buf subjects clips fill precision in
  t

let inflate
    ?(join_type = JoinType.Round)
    ?(end_type = EndType.Polygon)
    ?(miter_limit = 2.0)
    ?(precision = 2)
    ~delta
    t
  =
  let buf, inflated = alloc () in
  let _ = C.Funcs.pathsd_inflate buf t delta join_type end_type miter_limit precision in
  inflated
