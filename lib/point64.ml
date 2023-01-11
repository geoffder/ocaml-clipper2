type t = C.Types.Point64.t

let make x y =
  let pt = Ctypes.make C.Types.Point64.t in
  Ctypes.setf pt C.Types.Point64.x x;
  Ctypes.setf pt C.Types.Point64.y y;
  pt

let to_tup t = Ctypes.getf t C.Types.Point64.x, Ctypes.getf t C.Types.Point64.y
