type t = C.Types.PointD.t

let make x y =
  let pt = Ctypes.make C.Types.PointD.t in
  Ctypes.setf pt C.Types.PointD.x x;
  Ctypes.setf pt C.Types.PointD.y y;
  pt

let to_tup t = Ctypes.getf t C.Types.PointD.x, Ctypes.getf t C.Types.PointD.y
let distance a b = C.Funcs.pointd_distance a b

let near_collinear a b c sin_sqrd_min_angle =
  C.Funcs.pointd_near_collinear a b c sin_sqrd_min_angle
