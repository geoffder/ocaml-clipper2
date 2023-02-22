type t = C.Types.Point64.t

let make x y =
  let pt = Ctypes.make C.Types.Point64.t in
  Ctypes.setf pt C.Types.Point64.x x;
  Ctypes.setf pt C.Types.Point64.y y;
  pt

let[@inline] x t = Ctypes.getf t C.Types.Point64.x
let[@inline] y t = Ctypes.getf t C.Types.Point64.y
let[@inline] to_tup t = x t, y t
