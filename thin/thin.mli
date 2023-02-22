type rectd
type pathd
type pathsd
type polytreed
type rect64
type path64
type paths64
type polytree64

module PointD : sig
  type t

  val make : float -> float -> t
  val x : t -> float
  val y : t -> float
end

module RectD : sig
  type t = rectd

  val make : l:float -> t:float -> r:float -> b:float -> t
  val of_pts : PointD.t -> PointD.t -> t
  val width : t -> float
  val height : t -> float
  val midpoint : t -> PointD.t
  val scale : float -> t -> t
  val as_path : t -> pathd
  val contains_pt : t -> PointD.t -> bool
  val contains_rect : t -> t -> bool
  val is_empty : t -> bool
  val intersects : t -> t -> bool
end

module PathD : sig
  type t = pathd

  val make : unit -> t
  val length : t -> int
  val get_point : t -> int -> PointD.t
  val add_point : t -> PointD.t -> unit
  val ellipse : ?fn:int -> ?centre:PointD.t -> float -> float -> t
  val translate : t -> float -> float -> t
  val bounds : t -> rectd
  val rect_clip : ?precision:int -> ?closed:bool -> t -> rectd -> pathsd
  val trim_collinear : ?precision:int -> ?closed:bool -> t -> t
  val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t
  val strip_duplicates : ?closed:bool -> t -> t
  val simplify : ?closed:bool -> ?eps:float -> t -> t
  val ramer_douglas_peucker : ?eps:float -> t -> t
  val area : t -> float
  val point_inside : t -> PointD.t -> [> `Inside | `OnBorder | `Outside ]
  val is_positive : t -> bool
  val minkowski_sum : ?precision:int -> ?closed:bool -> pattern:t -> t -> pathsd
  val minkowski_diff : ?precision:int -> ?closed:bool -> pattern:t -> t -> pathsd
end

module PathsD : sig
  type t = pathsd

  val make : unit -> t
  val add_path : t -> PathD.t -> unit
  val length : t -> int
  val path_length : t -> int -> int
  val get_path : t -> int -> PathD.t
  val get_point : t -> int -> int -> PointD.t
  val translate : t -> float -> float -> t

  val boolean_op
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> ?precision:int
    -> op:[< `Difference | `Intersection | `None | `Union | `Xor ]
    -> t
    -> t
    -> t

  val boolean_op_tree
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> ?precision:int
    -> op:[< `Difference | `Intersection | `None | `Union | `Xor ]
    -> t
    -> t
    -> polytreed

  val intersect
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> ?precision:int
    -> t
    -> t
    -> t

  val union
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> ?precision:int
    -> t
    -> t

  val difference
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> ?precision:int
    -> t
    -> t
    -> t

  val xor
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> ?precision:int
    -> t
    -> t
    -> t

  val bounds : t -> rectd
  val rect_clip : ?precision:int -> ?closed:bool -> t -> RectD.t -> t

  val inflate
    :  ?precision:int
    -> ?join_type:[< `Miter of float option | `Round | `Square > `Round ]
    -> ?end_type:[< `Butt | `Joined | `Polygon | `Round | `Square > `Polygon ]
    -> delta:float
    -> t
    -> t

  val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t
  val strip_duplicates : ?closed:bool -> t -> t
  val simplify : ?closed:bool -> ?eps:float -> t -> t
  val ramer_douglas_peucker : ?eps:float -> t -> t
  val area : t -> float

  val minkowski_sum
    :  ?precision:int
    -> ?closed:bool
    -> ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> pattern:PathD.t
    -> t
    -> t

  val minkowski_diff
    :  ?precision:int
    -> ?closed:bool
    -> ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> pattern:PathD.t
    -> t
    -> t
end

module PolyTreeD : sig
  type t = polytreed

  val make : ?parent:t -> unit -> t
  val clear : t -> unit
  val set_inv_scale : t -> float -> unit
  val inv_scale : t -> float
  val add_child : t -> path64 -> t
  val parent : t -> t option
  val child : t -> int -> t
  val level : t -> int
  val count : t -> int
  val is_hole : t -> bool
  val area : t -> float
  val polygon : t -> PathD.t
  val decompose : (PathD.t -> 'a) -> t -> 'a Seq.t Seq.t
end

module ClipperD : sig
  type t

  val make : ?precision:int -> unit -> t
  val add_subject : t -> PathsD.t -> unit
  val add_open_subject : t -> PathsD.t -> unit
  val add_clip : t -> PathsD.t -> unit

  val execute
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> ?open_solution:PathsD.t
    -> op:[< `Difference | `Intersection | `None | `Union | `Xor ]
    -> solution:PathsD.t
    -> t
    -> (unit, string) result

  val execute_tree
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> ?open_solution:PathsD.t
    -> op:[< `Difference | `Intersection | `None | `Union | `Xor ]
    -> solution:PolyTreeD.t
    -> t
    -> (unit, string) result

  val get_preserve_collinear : t -> bool
  val set_preserve_collinear : t -> bool -> unit
  val get_reverse_solution : t -> bool
  val set_reverse_solution : t -> bool -> unit
  val clear : t -> unit
end

module Point64 : sig
  type t

  val make : int64 -> int64 -> t
  val x : t -> int64
  val y : t -> int64
end

module Rect64 : sig
  type t = rect64

  val make : l:int64 -> t:int64 -> r:int64 -> b:int64 -> t
  val of_pts : Point64.t -> Point64.t -> t
  val width : t -> int64
  val height : t -> int64
  val midpoint : t -> Point64.t
  val scale : float -> t -> t
  val as_path : t -> path64
  val contains_pt : t -> Point64.t -> bool
  val contains_rect : t -> t -> bool
  val is_empty : t -> bool
  val intersects : t -> t -> bool
end

module Path64 : sig
  type t = path64

  val make : unit -> t
  val length : t -> int
  val get_point : t -> int -> Point64.t
  val add_point : t -> Point64.t -> unit
  val ellipse : ?fn:int -> ?centre:Point64.t -> int64 -> int64 -> t
  val translate : t -> int64 -> int64 -> t
  val bounds : t -> rect64
  val rect_clip : ?closed:bool -> t -> rect64 -> paths64
  val trim_collinear : ?closed:bool -> t -> t
  val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t
  val strip_duplicates : ?closed:bool -> t -> t
  val simplify : ?closed:bool -> ?eps:float -> t -> t
  val ramer_douglas_peucker : ?eps:float -> t -> t
  val area : t -> float
  val point_inside : t -> Point64.t -> [> `Inside | `OnBorder | `Outside ]
  val is_positive : t -> bool
  val minkowski_sum : ?closed:bool -> pattern:t -> t -> paths64
  val minkowski_diff : ?closed:bool -> pattern:t -> t -> paths64
end

module Paths64 : sig
  type t = paths64

  val make : unit -> t
  val add_path : t -> Path64.t -> unit
  val length : t -> int
  val path_length : t -> int -> int
  val get_path : t -> int -> Path64.t
  val get_point : t -> int -> int -> Point64.t
  val translate : t -> int64 -> int64 -> t

  val boolean_op
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> op:[< `Difference | `Intersection | `None | `Union | `Xor ]
    -> t
    -> t
    -> t

  val boolean_op_tree
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> op:[< `Difference | `Intersection | `None | `Union | `Xor ]
    -> t
    -> t
    -> polytree64

  val intersect
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> t
    -> t
    -> t

  val union
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> t
    -> t

  val difference
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> t
    -> t
    -> t

  val xor
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> t
    -> t
    -> t

  val bounds : t -> rect64
  val rect_clip : ?closed:bool -> t -> Rect64.t -> t

  val inflate
    :  ?join_type:[< `Miter of float option | `Round | `Square > `Round ]
    -> ?end_type:[< `Butt | `Joined | `Polygon | `Round | `Square > `Polygon ]
    -> delta:float
    -> t
    -> t

  val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t
  val strip_duplicates : ?closed:bool -> t -> t
  val simplify : ?closed:bool -> ?eps:float -> t -> t
  val ramer_douglas_peucker : ?eps:float -> t -> t
  val area : t -> float

  val minkowski_sum
    :  ?closed:bool
    -> ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> pattern:Path64.t
    -> t
    -> t

  val minkowski_diff
    :  ?closed:bool
    -> ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> pattern:Path64.t
    -> t
    -> t
end

module PolyTree64 : sig
  type t = polytree64

  val make : ?parent:t -> unit -> t
  val clear : t -> unit
  val add_child : t -> path64 -> t
  val parent : t -> t option
  val child : t -> int -> t
  val level : t -> int
  val count : t -> int
  val is_hole : t -> bool
  val area : t -> float
  val polygon : t -> Path64.t
  val decompose : (Path64.t -> 'a) -> t -> 'a Seq.t Seq.t
end

module Clipper64 : sig
  type t

  val make : unit -> t
  val add_subject : t -> Paths64.t -> unit
  val add_open_subject : t -> Paths64.t -> unit
  val add_clip : t -> Paths64.t -> unit

  val execute
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> ?open_solution:Paths64.t
    -> op:[< `Difference | `Intersection | `None | `Union | `Xor ]
    -> solution:Paths64.t
    -> t
    -> (unit, string) result

  val execute_tree
    :  ?fill_rule:[< `EvenOdd | `Negative | `NonZero | `Positive > `NonZero ]
    -> ?open_solution:Paths64.t
    -> op:[< `Difference | `Intersection | `None | `Union | `Xor ]
    -> solution:PolyTree64.t
    -> t
    -> (unit, string) result

  val get_preserve_collinear : t -> bool
  val set_preserve_collinear : t -> bool -> unit
  val get_reverse_solution : t -> bool
  val set_reverse_solution : t -> bool -> unit
  val clear : t -> unit
end

module Offset : sig
  type t

  val make
    :  ?miter_limit:float
    -> ?arc_tolerance:float
    -> ?preserve_collinear:bool
    -> ?reverse_solution:bool
    -> unit
    -> t

  val add_pathd
    :  ?join_type:[< `Miter of float option | `Round | `Square > `Round ]
    -> ?end_type:[< `Butt | `Joined | `Polygon | `Round | `Square > `Polygon ]
    -> t
    -> PathD.t
    -> unit

  val add_pathsd : t -> PathsD.t -> C__.Types.JoinType.t -> C__.Types.EndType.t -> unit
  val add_path64 : t -> Path64.t -> C__.Types.JoinType.t -> C__.Types.EndType.t -> unit
  val add_paths64 : t -> Paths64.t -> C__.Types.JoinType.t -> C__.Types.EndType.t -> unit

  val execute
    :  unit Ctypes_static.ptr
    -> t
    -> float
    -> C__.Types.Paths64.t Ctypes_static.ptr

  val get_miter_limit : t -> float
  val set_miter_limit : t -> float -> unit
  val get_arc_tolerance : t -> float
  val set_arc_tolerance : t -> float -> unit
  val get_preserve_collinear : t -> bool
  val set_preserve_collinear : t -> bool -> unit
  val get_reverse_solution : t -> bool
  val set_reverse_solution : t -> bool -> unit
  val error_code : t -> int
  val clear : t -> unit
end
