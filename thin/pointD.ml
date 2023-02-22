type t = C.Types.PointD.t

let make x y =
  let pt = Ctypes.make C.Types.PointD.t in
  Ctypes.setf pt C.Types.PointD.x x;
  Ctypes.setf pt C.Types.PointD.y y;
  pt

let[@inline] x t = Ctypes.getf t C.Types.PointD.x
let[@inline] y t = Ctypes.getf t C.Types.PointD.y
let[@inline] to_tup t = x t, y t
