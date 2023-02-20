module type V = sig
  (** Signature of a 2d vector type to be used to represent
       points for the construction and destruction of Clipper2 path types *)

  (** numeric type for elements of the vector -- will be destructed (type
       declaration not required in user supplied module) *)
  type n

  (** 2d vector type *)
  type t

  (** [v x y] conststructs a vector from [x] and [y] coordinates *)
  val v : n -> n -> t

  (** [x t] obtains the x coordinate of the vector [t] *)
  val x : t -> n

  (** [y t] obtains the y coordinate of the vector [t] *)
  val y : t -> n
end

module type Poly = sig
  (** Signature of a polygon type -- an outer path and zero or more inner paths
    (holes). To be used for construction and destruction of Clipper2 paths
    types. *)

  (** 2d vector type representing points -- will be destructed (type declaration
       not required in user supplied module) *)
  type v

  (** polygon type *)
  type t

  (** [to_list t] converts the polygon [t] to a list of lists of points *)
  val to_list : t -> v list list

  (** [of_list vs] creates a polygon from the list of lists of points [vs] *)
  val of_list : v list list -> t
end

module ConfigTypes = struct
  (** Clipping types for boolean operations. See Clipper2's docs for
       {{:http://www.angusj.com/clipper2/Docs/Units/Clipper/Types/ClipType.htm}
       visual demonstrations}. *)
  type clip_type =
    [ `None
    | `Intersection (** AND -- regions covered by both subject and clip polygons *)
    | `Union
      (** OR -- regions covered by subject or clip polygons, or both
                   polygons *)
    | `Difference (** NOT -- regions covered by subject, but not clip polygons *)
    | `Xor
      (** exclusive or -- regions covered by subject or clip polygons, but
                 not both *)
    ]

  (** Filling rules used by the clipping algorithm for boolean operations. See
       Clipper2's docs for a detailed
       {{:http://www.angusj.com/clipper2/Docs/Units/Clipper/Types/FillRule.htm}
       explanation} of how they differ). *)
  type fill_rule =
    [ `EvenOdd (** only odd numbered sub-regions are filled *)
    | `NonZero (** non-zero sub-regions are filled *)
    | `Positive (** only sub-regions with winding counts [> 0] are filled *)
    | `Negative (** only sub-regions with winding counts [< 0] are filled *)
    ]

  (** Defines the treatment of corners when offsetting paths. Visual examples
       are available in the Clipper2
       {{:http://www.angusj.com/clipper2/Docs/Units/Clipper/Types/JoinType.htm}
       docs}. *)
  type join_type =
    [ `Square
      (** squaring applied uniformally at all joins where the {i internal}
             join angle is less than 90 degrees. The squared edg will be at
             exactly the offset distance from the join vertex *)
    | `Round
      (** rounding is appliedto all joins that have convex external
             angles, and it maintains the exact offset distance from the join vertex *)
    | `Miter of float option
      (** there's a necessary limit to mitered joins (to avoid narrow angled
              joins producing excessively long and narrow
              {{:http://www.angusj.com/clipper2/Docs/Units/Clipper.Offset/Classes/ClipperOffset/Properties/MiterLimit.htm}
              spikes})). The optional limit sets the maximum distance in multiples of
              the [delta] specified for the offsetting operation (default is
              [2], which is the minimum allowed). *)
    ]

  (** Sets whether paths are treated as closed ([`Polygon]) when offsetting or
       open (and how to do so, if so). Visual examples are available in the Clipper2
      {{:http://www.angusj.com/clipper2/Docs/Units/Clipper/Types/EndType.htm}
      docs}. *)
  type end_type =
    [ `Polygon (** paths are assumed to be closed and treated as polygons *)
    | `Joined (** ends are joined and the paths are treated as polylines *)
    | `Butt (** ends are squared off without any extrusion *)
    | `Square (** ends extend the offset amount while being {i squared off} *)
    | `Round (** ends extend the offset amount while being {i rounded off} *)
    ]
end

module type Config = sig
  (** Configuration parameters allowing user control Clipper2 interface defaults *)

  (** The default filling rule used by the clipping algorithm (for boolean
       operations) *)
  val fill_rule : ConfigTypes.fill_rule option

  (** The default treatment of corners when offsetting paths *)
  val join_type : ConfigTypes.join_type option

  (** The default for whether paths are treated as closed (polygons), or as
       open paths (and if so, what open path strategy to take) *)
  val end_type : ConfigTypes.end_type option

  (** The number of significant decimal places to use during numerical
       operations (if using the float interface). Can be up to [8]. *)
  val precision : int option

  (** The default epsilon value to use for relevant functions. *)
  val eps : float option
end

module type S = sig
  (** numeric type matching the elements of {!type:v} *)
  type n

  (** 2d vector type representing points *)
  type v

  (** polygon type -- outer path and zero or more inner paths (holes) *)
  type poly

  type cpath
  type cpaths

  include module type of ConfigTypes

  type ('cpp, 'list) t =
    | Path : cpath -> ([ `Path ], v list) t
    | Paths : cpaths -> ([ `Paths ], v list list) t

  (** The Clipper2 Path type (std::vector of point) *)
  type path = ([ `Path ], v list) t

  (** The Clipper2 paths type (std::vector of path)

       These lists of contours are not organized hierarchically (by
       parent-child / outer-hole) relationships, and may include any number
       of open paths or polygons. *)
  type paths = ([ `Paths ], v list list) t

  module Rect : sig
    (** an axis-aligned rectangle (Clipper2 class) *)
    type t

    (** {1 Construction / Conversion} *)

    (** [make ~l ~t ~r ~b]

          Create an axis-aligned rectangle with the bounds [l]eft, [t]op,
          [r]ight, and [b]ottom. *)
    val make : l:n -> t:n -> r:n -> b:n -> t

    (** [of_pts a b]

          Create an axis-aligned bounding box (rectangle) that contains the
          points [a] and [b]. *)
    val of_pts : v -> v -> t

    (** [as_path t]

          Obtain a path describing the perimeter of the rectangle [t]. *)
    val as_path : t -> path

    (** {1 Transformation} *)

    (** [scale t s]

          Scale the rectangle [t] by the factor [s]. *)
    val scale : float -> t -> t

    (** {1 Geometry} *)

    (** [width t]

          Obtain the width of the rectangle [t]. *)
    val width : t -> n

    (** [height t]

          Obtain the height of the rectangle [t]. *)
    val height : t -> n

    (** [midpoint t]

          Obtain the midpoint of the rectangle [t]. *)
    val midpoint : t -> v

    (** [contains_pt t p]

          Determine whether the point [p] lies within the rectangle [t]. *)
    val contains_pt : t -> v -> bool

    (** [contains_rect a b]

          Determine whether the rectangle [a] fully contains the rectangle [b]. *)
    val contains_rect : t -> t -> bool

    (** [intersects a b]

          Determine whether the rectangles [a] and [b] intersect. *)
    val intersects : t -> t -> bool

    (** [is_empty t]

          Check whether the rectangle [t] has an area of zero. *)
    val is_empty : t -> bool
  end

  (** {1 Construction / Conversion} *)

  (** [of_list ps]

       Create a path (or paths) from the list(s) of 2d points [ps]. *)
  val of_list : 'list -> (_, 'list) t

  (** [to_list t]

       Convert the path (or paths) [t] to a list(s) of 2d points. *)
  val to_list : 'list -> (_, 'list) t

  (** [of_poly p]

          Create a paths from a polygon [p]. *)
  val of_poly : poly -> paths

  (** [of_polys ps]

          Create a paths from a list of polygons [ps]. *)
  val of_polys : poly list -> paths

  (** [to_polys ?fill_rule t]

          Extract a list of non-overlapping polygons from the set of paths [t].
          This involves a clipper union operation tracking the parent-child
          (outline-hole) relationships of the paths, thus [fill_rule] can be
          provided to override the default rule if desired. *)
  val to_polys : ?fill_rule:fill_rule -> ('cpp, 'list) t -> poly list

  (** [ellipse ?fn ?centre rs]

        Draw an elliptical path with with the specified xy radii [rs] centred
        on the origin, or at the point [centre] if provided. The number
        of segments can be set explicitly with [fn], otherwise the quality is
        calculated based on the dimensions. *)
  val ellipse : ?fn:int -> ?centre:v -> v -> path

  (** {1 Access} *)

  (** [length t]

       Return the number of points in the path [t]. *)
  val length : path -> int

  (** [path_point t i]

       Get the point at index [i] from the path [t]. *)
  val path_point : path -> int -> v

  (** [paths_point t i]

       Get the point at index [j] from the path at index [i] of [t]. *)
  val paths_point : paths -> int -> int -> v

  val ( .%() ) : path -> int -> v
  val ( .%{} ) : paths -> int -> int -> v

  (** {1 Boolean Operations} *)

  (** [boolean_op ?fill_rule ~op a b]

          Perform the boolean operation [op] with the specified [fill_rule]
          on the polygons (closed paths) [a] and [b]. *)
  val boolean_op
    :  ?fill_rule:fill_rule
    -> op:clip_type
    -> ('cpp, 'list) t
    -> ('cpp, 'list) t
    -> paths

  (** [intersect ?fill_rule a b]

          Intersect the polygons [a] and [b] according to [fill_rule]. The
          result includes regions covered by both polygons. *)
  val intersect : ?fill_rule:fill_rule -> ('cpp, 'list) t -> ('cpp, 'list) t -> paths

  (** [union ?fill_rule subjects]

          Union the polygons [subjects] according to [fill_rule]. The result
          includes the regions covered by any of the polygons contained in
          [subjects]. *)
  val union : ?fill_rule:fill_rule -> ('cpp, 'list) t list -> paths

  (** [add ?fill_rule a b]

          {!union} the polygon [a] and [b]. *)
  val add : ?fill_rule:fill_rule -> ('cpp, 'list) t -> ('cpp, 'list) t -> paths

  (** [difference ?fill_rule a b]

          Difference the polygon [b] from the polygon [a] according to
          [fill_rule]. The result includes the regions covered by the polygon [a],
          but not the polygon [b]. *)
  val difference : ?fill_rule:fill_rule -> ('cpp, 'list) t -> ('cpp, 'list) t -> paths

  (** [sub a b]

           Difference the polygon [b] from [a] (alias to {!difference}). *)
  val sub : ?fill_rule:fill_rule -> ('cpp, 'list) t -> ('cpp, 'list) t -> paths

  (** [xor ?fill_rule subjects clips]

          Perform the exclusive-or boolean operation between the closed ps
          [a] and [b] according to [fill_rule]. The result includes
          regions covered by the either [a] or [b], but not both. *)
  val xor : ?fill_rule:fill_rule -> ('cpp, 'list) t -> ('cpp, 'list) t -> paths

  (** [rect_clip ?closed r t]

          Intersect the path [t] with the axis-aligned rectangle [r]. The path is
          treated as closed/polygonal by default, but an open path may be clipped
          by setting [~closed:false]. *)
  val rect_clip : ?closed:bool -> Rect.t -> ('cpp, 'list) t -> paths

  (** {1 Offsetting} *)

  (** [inflate ?join_type ?end_type ~delta t]

          Offset the polygon (or open path) [t] by [delta]. If [t] is a closed
          polygonal path, it's important that [end_type] is [`Polygon] (default
          if not overridden by user's [Config]). If instead you select one of the open
          path end types (e.g. [`Joined]), the polygon's {i outline} will be inflated
          ({{:https://github.com/AngusJohnson/Clipper2/discussions/154#discussion-4284428}
          example}).

          - with closed paths (polygons), a positive delta specifies how much outer
            polygon contours will expand and how much inner "hole" contours will contract (and
            the converse with negative deltas).
          - with open paths (polylines), including EndType.Join, delta specifies the width
            of the inflated line.
          - {b Caution:} offsetting self-intersecting polygons may produce
            unexpected results. *)
  val inflate
    :  ?join_type:join_type
    -> ?end_type:end_type
    -> delta:float
    -> ('cpp, 'list) t
    -> paths

  (** {1 Minkowski} *)

  (** [minkowski_sum ?closed ~pattern t]

        Apply {{:https://en.wikipedia.org/wiki/Minkowski_addition} Minkowski
        addition} of the path [pattern] to the path [t]. [t] is treated
        as a [closed] polygon unless otherwise specified. *)
  val minkowski_sum : ?closed:bool -> pattern:path -> ('cpp, 'list) t -> ('cpp, 'list) t

  (** [minkowski_diff ?closed ~pattern t]

        Apply {{:https://en.wikipedia.org/wiki/Minkowski_addition} Minkowski
        subtraction} of the path [pattern] from the path [t]. [t] is treated
        as a [closed] polygon unless otherwise specified. *)
  val minkowski_diff : ?closed:bool -> pattern:path -> ('cpp, 'list) t -> ('cpp, 'list) t

  (** {1 Path Simplification} *)

  (** [simplify ?closed ?eps t]

       Remove extraneous vertices from the path [t] (similar to
       {!ramer_douglas_peucker}). *)
  val simplify : ?closed:bool -> ?eps:float -> ('cpp, 'list) t -> ('cpp, 'list) t

  (** [ramer_douglas_peucker ?eps t]

       Applies the
       {{:https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm}
       Ramer-Douglas-Peucker algorithm} to remove extraneous vertices from the
       path [t]. Put simply, vertices will be removed if they are less than
       [eps] distance from imaginary lines passing through their adjacent vertices.

       This function is particularly useful following offsetting (ie
       inflating/shrinking paths). Offsetting often creates tiny segments that don't
       improve path quality. Further these tiny segments create angles that are
       strongly influenced by integer rounding. While these tiny segments are too
       small to be noticeable following a single offset procedure, they're likely to
       degrade the quality of subsequent offsets. And they'll also degrade
       performance. Because of this, it is strongly recommended calling this function
       after every polygon offset. *)
  val ramer_douglas_peucker : ?eps:float -> ('cpp, 'list) t -> ('cpp, 'list) t

  (** [trim_collinear ?closed t]

          Remove collinear points (that fall on a line drawn between their
          neighbours) from the path [t]. The path is treated as [closed] by
          default. *)
  val trim_collinear : ?closed:bool -> ('cpp, 'list) t -> ('cpp, 'list) t

  (** [strip_near_equal ?closed ?eps t]

       Remove adjacent points that are less than [eps] distance apart from
       their neighbour from the paths [t]. The path is treated as [closed] by
       default. *)
  val strip_near_equal : ?closed:bool -> ?eps:float -> ('cpp, 'list) t -> ('cpp, 'list) t

  (** [strip_duplicates ?closed ?eps t]

       Remove adjacent points that duplicate of their neighbours from the
       paths [t]. The path is treated as [closed] by default. *)
  val strip_duplicates : ?closed:bool -> ('cpp, 'list) t -> ('cpp, 'list) t

  (** {1 Transformation} *)

  (** [translate v t]

       Translate the path [t] along the vector [v]. *)
  val translate : v -> ('cpp, 'list) t -> ('cpp, 'list) t

  (** {1 Geometry} *)

  (** [area t]

       Compute the signed area of the path [t]. *)
  val area : ('cpp, 'list) t -> float

  (** [bounds t]

          Compute the axis-aligned bounding box that contains the path [t]. *)
  val bounds : ('cpp, 'list) t -> Rect.t

  (** [is_positive t]

       Check if the orientation/winding of the path [t] is positive. *)
  val is_positive : path -> bool

  (** [point_inside t p]

       Determine whether the point [p] is inside, outside, or on the border of
       [t]. *)
  val point_inside : path -> v -> [> `Inside | `OnBorder | `Outside ]
end

module type Intf = sig
  (** {1 Functor Parameters }*)

  module type V = V
  module type Poly = Poly

  (** {2 Configuration} *)

  include module type of ConfigTypes

  module type Config = Config

  (** [config ?fill_rule ?join_type ?end_type ?precision ?eps ()]

       Construct a {!module-type:Config} functor argument to set defaults
       appropriate to your use case.

      {[
        MakeD (V) ((val config ()))
      ]}

       - [fill_rule] sets the default filling rule used by the clipping
         algorithm used for boolean operations
         (default is [`NonZero], as used by {{:https://openscad.org/} OpenSCAD}).
       - [join_type] defines the treatment of corners when offsetting paths
         (default is [`Round]).
       - [end_type] sets whether paths are treated as closed (polygons) when
         offsetting (as with the default [`Polygon]) or open (and how to do so,
         if so).
       - [precision] sets the number of decimals of precision used for
         operations that are performed with [int64] internally in Clipper2
         (default is [2], up to [8] is allowed). This is only relevant if using
         the decimal interface.
       - [eps] sets the default epsilon value used for the functions that take
         it as argument. If not provided, this defaults to the smallest relevant
         significant figure as determined by [precision] in the case of the
         decimal interface ({i e.g.} [0.01] when [precision = 2]), or simply [1]
         for the [int64] interface. *)
  val config
    :  ?fill_rule:fill_rule
    -> ?join_type:join_type
    -> ?end_type:end_type
    -> ?precision:int
    -> ?eps:float
    -> unit
    -> (module Config)

  (** {1 Clipper Instance} *)

  (** The signature of Clipper2 binding modules produced by the provided [Make]
       functors *)
  module type S = S

  (** {1 Functors} *)

  (** [MakeD' (V) (P) (C)] creates a Clipper2 module with the 2d [float] vector
       [V], the polygon type [P] (composed of [V.t]s, used for input/output), and a
       user configuration (see {!config} for convenience constructor). *)
  module MakeD' : functor
    (V : V with type n := float)
    (P : Poly with type v := V.t)
    (_ : Config)
    -> S with type n := float and type v := V.t and type poly := P.t

  (** [MakeD (V) (C)] creates a Clipper2 module with the 2d [float] vector [V], and a user
       configuration (see {!config} for convenience constructor). Same as
       {!MakeD'}, but the polygon type is preset to [V.t list list]. *)
  module MakeD : functor (V : V with type n := float) (_ : Config) ->
    S with type n := float and type v := V.t and type poly := V.t list list

  (** [MakeD' (V) (P) (C)] creates a Clipper2 module with the 2d [int64] vector
       [V], the polygon type [P] (composed of [V.t]s, used for input/output), and a
       user configuration (see {!config} for convenience constructor). *)
  module Make64' : functor
    (V : V with type n := int64)
    (P : Poly with type v := V.t)
    (_ : Config)
    -> S with type n := int64 and type v := V.t and type poly := P.t

  (** [MakeD (V) (C)] creates a Clipper2 module with the 2d [int64] vector [V], and a user
       configuration (see {!config} for convenience constructor). Same as
       {!MakeD'}, but the polygon type is preset to [V.t list list]. *)
  module Make64 : functor (V : V with type n := int64) (_ : Config) ->
    S with type n := int64 and type v := V.t and type poly := V.t list list
end
