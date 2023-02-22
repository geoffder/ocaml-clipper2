open Ctypes
open C.Types

let voidp_coerce typ = Ctypes.coerce Ctypes_static.(ptr void) typ

let string_to_ptr ctyp s =
  Ctypes.(CArray.of_string s |> CArray.start |> coerce (ptr char) (ptr ctyp))

let size_of_int = Unsigned.Size_t.of_int
let size_to_int = Unsigned.Size_t.to_int
let point64_to_tup p = getf p Point64.x, getf p Point64.y

let point64_of_tup (x, y) =
  let pt = make Point64.t in
  setf pt Point64.x x;
  setf pt Point64.y y;
  pt

let pointd_to_tup p = getf p PointD.x, getf p PointD.y

let pointd_of_tup (x, y) =
  let pt = make PointD.t in
  setf pt PointD.x x;
  setf pt PointD.y y;
  pt
