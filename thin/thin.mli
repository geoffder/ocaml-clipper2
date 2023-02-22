(**/**)

type rectd
type pathd
type pathsd
type polytreed
type rect64
type path64
type paths64
type polytree64

(**/**)

(** {1 Configuration Types} *)

include module type of ConfigTypes

(** {1 Decimal Interface} *)

module PointD : sig
  (** 2d points (float) *)

  type t

  (** {1 Construction / Conversion} *)

  val make : float -> float -> t
  val to_tup : t -> float * float

  (** {1 Access} *)

  val x : t -> float
  val y : t -> float
end

module RectD : sig
  (** An axis-aligned rectangle used bounding box computations and quick
       rectangular clipping (boolean intersection) operations. (see
       {!val:PathsD.rect_clip}) *)

  type t = rectd

  (** {1 Construction and Conversion} *)

  (** [make ~l ~t ~r ~b]

        Create an axis-aligned rectangle with the bounds [l]eft, [t]op,
        [r]ight, and [b]ottom. *)
  val make : l:float -> t:float -> r:float -> b:float -> t

  (** [of_pts a b]

       Create an axis-aligned bounding box (rectangle) that contains the
       points [a] and [b]. *)
  val of_pts : PointD.t -> PointD.t -> t

  (** [as_path t]

       Obtain a path describing the perimeter of the rectangle [t]. *)
  val as_path : t -> pathd

  (** {1 Access} *)

  (** [width t]

       Obtain the width of the rectangle [t]. *)
  val width : t -> float

  (** [height t]

       Obtain the height of the rectangle [t]. *)
  val height : t -> float

  (** [midpoint t]

       Obtain the midpoint of the rectangle [t]. *)
  val midpoint : t -> PointD.t

  (** {1 Transformations} *)

  (** [scale t s]

       Scale the rectangle [t] by the factor [s]. *)
  val scale : float -> t -> t

  (** {1 Geometry} *)

  (** [contains_pt t p]

       Determine whether the point [p] lies within the rectangle [t]. *)
  val contains_pt : t -> PointD.t -> bool

  (** [contains_rect a b]

       Determine whether the rectangle [a] fully contains the rectangle [b]. *)
  val contains_rect : t -> t -> bool

  (** [is_empty t]

       Check whether the rectangle [t] has an area of zero. *)
  val is_empty : t -> bool

  (** [intersects a b]

       Determine whether the rectangles [a] and [b] intersect. *)
  val intersects : t -> t -> bool
end

module PathD : sig
  (** Rectangular clipping, simplification, and other transformations on
       sequences of vertices (float) defining a single contour (open or closed
       path). *)

  (** the Clipper2 path type (std::vector of point) *)
  type t = pathd

  (** {1 Construction} *)

  val make : unit -> t
  val ellipse : ?fn:int -> ?centre:PointD.t -> float -> float -> t
  val add_point : t -> PointD.t -> unit

  (** {1 Access} *)

  val length : t -> int
  val unsafe_get : t -> int -> PointD.t
  val get : t -> int -> PointD.t

  (** {1 Transformation} *)

  val translate : t -> float -> float -> t

  (** {1 Rectangular Clipping} *)

  val rect_clip : ?precision:int -> ?closed:bool -> t -> rectd -> pathsd

  (** {1 Simplification} *)

  val trim_collinear : ?precision:int -> ?closed:bool -> t -> t
  val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t
  val strip_duplicates : ?closed:bool -> t -> t
  val simplify : ?closed:bool -> ?eps:float -> t -> t
  val ramer_douglas_peucker : ?eps:float -> t -> t

  (** {1 Minkowski} *)

  val minkowski_sum : ?precision:int -> ?closed:bool -> pattern:t -> t -> pathsd
  val minkowski_diff : ?precision:int -> ?closed:bool -> pattern:t -> t -> pathsd

  (** {1 Geometry} *)

  val area : t -> float
  val bounds : t -> rectd
  val point_inside : t -> PointD.t -> [> `Inside | `OnBorder | `Outside ]
  val is_positive : t -> bool
end

module PathsD : sig
  (** Clipping (boolean), offseting, simplification, and minkowski operations on sequences
       of {!PathD.t}. *)

  (** The Clipper2 paths type (std::vector of path)

       These lists of contours are not organized hierarchically (by
       parent-child / outer-hole) relationships, and may include any number
       of open paths or polygons. *)
  type t = pathsd

  (** {1 Construction} *)

  val make : unit -> t
  val add_path : t -> PathD.t -> unit

  (** {1 Access} *)

  val length : t -> int
  val get : t -> int -> int -> PointD.t
  val sublength : t -> int -> int
  val subpath : t -> int -> PathD.t
  val unsafe_sublength : t -> int -> int
  val unsafe_subpath : t -> int -> PathD.t
  val unsafe_get : t -> int -> int -> PointD.t

  (** {1 Transformation}*)

  val translate : t -> float -> float -> t

  (** {1 Clipping (Boolean) Operations} *)

  val boolean_op : ?fill_rule:fill_rule -> ?precision:int -> op:clip_type -> t -> t -> t

  val boolean_op_tree
    :  ?fill_rule:fill_rule
    -> ?precision:int
    -> op:clip_type
    -> t
    -> t
    -> polytreed

  val intersect : ?fill_rule:fill_rule -> ?precision:int -> t -> t -> t
  val union : ?fill_rule:fill_rule -> ?precision:int -> t -> t
  val difference : ?fill_rule:fill_rule -> ?precision:int -> t -> t -> t
  val xor : ?fill_rule:fill_rule -> ?precision:int -> t -> t -> t
  val rect_clip : ?precision:int -> ?closed:bool -> t -> RectD.t -> t

  (** {1 Offsetting} *)

  val inflate
    :  ?precision:int
    -> ?miter_limit:float
    -> ?join_type:join_type
    -> ?end_type:end_type
    -> delta:float
    -> t
    -> t

  (** {1 Simplification} *)

  val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t
  val strip_duplicates : ?closed:bool -> t -> t
  val simplify : ?closed:bool -> ?eps:float -> t -> t
  val ramer_douglas_peucker : ?eps:float -> t -> t

  (** {1 Minkowski} *)

  val minkowski_sum
    :  ?precision:int
    -> ?closed:bool
    -> ?fill_rule:fill_rule
    -> pattern:PathD.t
    -> t
    -> t

  val minkowski_diff
    :  ?precision:int
    -> ?closed:bool
    -> ?fill_rule:fill_rule
    -> pattern:PathD.t
    -> t
    -> t

  (** {1 Geometry} *)

  val area : t -> float
  val bounds : t -> rectd
end

module PolyTreeD : sig
  (** {!PolyTreeD.t} is a read-only data structure that receives solutions from
       clipping operations. It's an alternative to the {!PathsD.t} data structure which
       also receives solutions. However the principle advantage of {!PolyTreeD.t} over
       {!PathsD.t} is that it also represents the parent-child relationships of the
       polygons in the solution (where a parent's polygon will contain all its
       children polygons).

   The {!PolyTreeD.t} object that's to receive a clipping solution is passed as a
   parameter to {!ClipperD.execute}. When the clipping operation finishes, this
   object will be populated with data representing the clipped solution.

   A {!PolyTreeD.t} object is a container for any number of {!PolyTreeD.t} child objects,
   each representing a single polygon contour. The top-level descendents of a
   {!PolyTreeD.t} solution will always be outer polygon contours. Children may
   in turn contain their own children to any level of nesting. Children of outer
   polygon contours will always represent holes, and children of holes will
   always represent nested outer polygon contours.

   {!PolyTreeD.t} will never contain open paths since open paths
   can't contain paths. When clipping open paths, these will always be
   represented in solutions via a separate {!PathsD.t} structure. *)

  type t = polytreed

  val make : ?parent:t -> unit -> t
  val clear : t -> unit
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
  (** Clipping (boolean) operations on float paths/polygons *)

  (** the core class of Clipper2 (float edition) *)
  type t

  val make : ?precision:int -> unit -> t
  val add_subject : t -> PathsD.t -> unit
  val add_open_subject : t -> PathsD.t -> unit
  val add_clip : t -> PathsD.t -> unit

  val execute
    :  ?fill_rule:fill_rule
    -> ?open_solution:PathsD.t
    -> op:clip_type
    -> solution:PathsD.t
    -> t
    -> (unit, string) result

  val execute_tree
    :  ?fill_rule:fill_rule
    -> ?open_solution:PathsD.t
    -> op:clip_type
    -> solution:PolyTreeD.t
    -> t
    -> (unit, string) result

  val get_preserve_collinear : t -> bool
  val set_preserve_collinear : t -> bool -> unit
  val get_reverse_solution : t -> bool
  val set_reverse_solution : t -> bool -> unit
  val clear : t -> unit
end

(** {1 Int64 Interface} *)

module Point64 : sig
  (** 2d points (int64) *)

  type t

  (** {1 Construction / Conversion} *)

  val make : int64 -> int64 -> t
  val to_tup : t -> int64 * int64

  (** {1 Access} *)

  val x : t -> int64
  val y : t -> int64
end

module Rect64 : sig
  (** An axis-aligned rectangle used bounding box computations and quick
       rectangular clipping (boolean intersection) operations. (see
       {!val:Paths64.rect_clip}) *)

  type t = rect64

  (** {1 Construction and Conversion} *)

  (** [make ~l ~t ~r ~b]

        Create an axis-aligned rectangle with the bounds [l]eft, [t]op,
        [r]ight, and [b]ottom. *)
  val make : l:int64 -> t:int64 -> r:int64 -> b:int64 -> t

  (** [of_pts a b]

       Create an axis-aligned bounding box (rectangle) that contains the
       points [a] and [b]. *)
  val of_pts : Point64.t -> Point64.t -> t

  (** [as_path t]

       Obtain a path describing the perimeter of the rectangle [t]. *)
  val as_path : t -> path64

  (** {1 Access} *)

  (** [width t]

       Obtain the width of the rectangle [t]. *)
  val width : t -> int64

  (** [height t]

       Obtain the height of the rectangle [t]. *)
  val height : t -> int64

  (** [midpoint t]

       Obtain the midpoint of the rectangle [t]. *)
  val midpoint : t -> Point64.t

  (** {1 Transformations} *)

  (** [scale t s]

       Scale the rectangle [t] by the factor [s]. *)
  val scale : float -> t -> t

  (** {1 Geometry} *)

  (** [contains_pt t p]

       Determine whether the point [p] lies within the rectangle [t]. *)
  val contains_pt : t -> Point64.t -> bool

  (** [contains_rect a b]

       Determine whether the rectangle [a] fully contains the rectangle [b]. *)
  val contains_rect : t -> t -> bool

  (** [is_empty t]

       Check whether the rectangle [t] has an area of zero. *)
  val is_empty : t -> bool

  (** [intersects a b]

       Determine whether the rectangles [a] and [b] intersect. *)
  val intersects : t -> t -> bool
end

module Path64 : sig
  (** Rectangular clipping, simplification, and other transformations on
       sequences of vertices (int64) defining a single contour (open or closed
       path). *)

  (** the Clipper2 path type (std::vector of point) *)
  type t = path64

  (** {1 Construction} *)

  val make : unit -> t
  val ellipse : ?fn:int -> ?centre:Point64.t -> int64 -> int64 -> t
  val add_point : t -> Point64.t -> unit

  (** {1 Access} *)

  val length : t -> int
  val unsafe_get : t -> int -> Point64.t
  val get : t -> int -> Point64.t

  (** {1 Transformation} *)

  val translate : t -> int64 -> int64 -> t

  (** {1 Rectangular Clipping} *)

  val bounds : t -> rect64
  val rect_clip : ?closed:bool -> t -> rect64 -> paths64

  (** {1 Simplification} *)

  val trim_collinear : ?closed:bool -> t -> t
  val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t
  val strip_duplicates : ?closed:bool -> t -> t
  val simplify : ?closed:bool -> ?eps:float -> t -> t
  val ramer_douglas_peucker : ?eps:float -> t -> t

  (** {1 Minkowski} *)

  val minkowski_sum : ?closed:bool -> pattern:t -> t -> paths64
  val minkowski_diff : ?closed:bool -> pattern:t -> t -> paths64

  (** {1 Geometry} *)

  val area : t -> float
  val point_inside : t -> Point64.t -> [> `Inside | `OnBorder | `Outside ]
  val is_positive : t -> bool
end

module Paths64 : sig
  (** Clipping (boolean), offseting, simplification, and minkowski operations on sequences
       of {!Path64.t} *)

  (** The Clipper2 paths type (std::vector of path)

       These lists of contours are not organized hierarchically (by
       parent-child / outer-hole) relationships, and may include any number
       of open paths or polygons. *)
  type t = paths64

  (** {1 Construction} *)

  val make : unit -> t
  val add_path : t -> Path64.t -> unit

  (** {1 Access} *)

  val length : t -> int
  val get : t -> int -> int -> Point64.t
  val sublength : t -> int -> int
  val subpath : t -> int -> Path64.t
  val unsafe_get : t -> int -> int -> Point64.t
  val unsafe_sublength : t -> int -> int
  val unsafe_subpath : t -> int -> Path64.t

  (** {1 Transformations} *)

  val translate : t -> int64 -> int64 -> t

  (** {1 Clipping (Boolean) Operations} *)

  val boolean_op : ?fill_rule:fill_rule -> op:clip_type -> t -> t -> t
  val boolean_op_tree : ?fill_rule:fill_rule -> op:clip_type -> t -> t -> polytree64
  val intersect : ?fill_rule:fill_rule -> t -> t -> t
  val union : ?fill_rule:fill_rule -> t -> t
  val difference : ?fill_rule:fill_rule -> t -> t -> t
  val xor : ?fill_rule:fill_rule -> t -> t -> t
  val rect_clip : ?closed:bool -> t -> Rect64.t -> t

  (** {1 Offsetting} *)

  val inflate
    :  ?miter_limit:float
    -> ?join_type:join_type
    -> ?end_type:end_type
    -> delta:float
    -> t
    -> t

  (** {1 Simplification} *)

  val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t
  val strip_duplicates : ?closed:bool -> t -> t
  val simplify : ?closed:bool -> ?eps:float -> t -> t
  val ramer_douglas_peucker : ?eps:float -> t -> t

  (** {1 Minkowski} *)

  val minkowski_sum : ?closed:bool -> ?fill_rule:fill_rule -> pattern:Path64.t -> t -> t
  val minkowski_diff : ?closed:bool -> ?fill_rule:fill_rule -> pattern:Path64.t -> t -> t

  (** {1 Geometry} *)

  val area : t -> float
  val bounds : t -> rect64
end

module PolyTree64 : sig
  (** {!PolyTree64.t} is a read-only data structure that receives solutions from
       clipping operations. It's an alternative to the {!Paths64.t} data structure which
       also receives solutions. However the principle advantage of {!PolyTree64.t} over
       {!Paths64.t} is that it also represents the parent-child relationships of the
       polygons in the solution (where a parent's polygon will contain all its
       children polygons).

   The {!PolyTree64.t} object that's to receive a clipping solution is passed as a
   parameter to {!Clipper64.execute}. When the clipping operation finishes, this
   object will be populated with data representing the clipped solution.

   A {!PolyTree64.t} object is a container for any number of {!PolyTree64.t} child objects,
   each representing a single polygon contour. The top-level descendents of a
   {!PolyTree64.t} solution will always be outer polygon contours. Children may
   in turn contain their own children to any level of nesting. Children of outer
   polygon contours will always represent holes, and children of holes will
   always represent nested outer polygon contours.

   {!PolyTree64.t} will never contain open paths since open paths
   can't contain paths. When clipping open paths, these will always be
   represented in solutions via a separate {!Paths64.t} structure. *)

  type t = polytree64

  val make : ?parent:t -> unit -> t
  val clear : t -> unit
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
  (** Clipping (boolean) operations on int64 paths/polygons *)

  (** the core class of Clipper2 (int64 edition) *)
  type t

  val make : unit -> t
  val add_subject : t -> Paths64.t -> unit
  val add_open_subject : t -> Paths64.t -> unit
  val add_clip : t -> Paths64.t -> unit

  val execute
    :  ?fill_rule:fill_rule
    -> ?open_solution:Paths64.t
    -> op:clip_type
    -> solution:Paths64.t
    -> t
    -> (unit, string) result

  val execute_tree
    :  ?fill_rule:fill_rule
    -> ?open_solution:Paths64.t
    -> op:clip_type
    -> solution:PolyTree64.t
    -> t
    -> (unit, string) result

  val get_preserve_collinear : t -> bool
  val set_preserve_collinear : t -> bool -> unit
  val get_reverse_solution : t -> bool
  val set_reverse_solution : t -> bool -> unit
  val clear : t -> unit
end

(** {1 Offsetting} *)

module Offset : sig
  (** This class provides a mixed interface (float and int64). Unlike the
    purpose built {!ClipperD.t} it does not perform handle the conversion of
    incoming doubles to int64 using a precision set by the user -- they are
    simply truncated. Thus if you are working with floats, it is recommended
    that you make use of the helper function {!PathsD.inflate} at this time. *)

  type t

  val make
    :  ?miter_limit:float
    -> ?arc_tolerance:float
    -> ?preserve_collinear:bool
    -> ?reverse_solution:bool
    -> unit
    -> t

  val add_pathd : ?join_type:join_type -> ?end_type:end_type -> t -> PathD.t -> unit
  val add_pathsd : ?join_type:join_type -> ?end_type:end_type -> t -> PathsD.t -> unit
  val add_path64 : ?join_type:join_type -> ?end_type:end_type -> t -> Path64.t -> unit
  val add_paths64 : ?join_type:join_type -> ?end_type:end_type -> t -> Paths64.t -> unit
  val execute : delta:float -> t -> Paths64.t
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
