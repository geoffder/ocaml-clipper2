module type VD = sig
  (** Signature of a 2d decimal ([float]) vector type to be used to represent
    points for the construction and destruction of Clipper2 path types *)

  (** 2d [float] vector type *)
  type t

  (** [v x y] conststructs a vector from [x] and [y] coordinates *)
  val v : float -> float -> t

  (** [x t] obtains the x coordinate of the vector [t] *)
  val x : t -> float

  (** [y t] obtains the y coordinate of the vector [t] *)
  val y : t -> float
end

module type V64 = sig
  (** Signature of a 2d [int64] vector type to be used to represent
    points for the construction and destruction of Clipper2 path types *)

  (** 2d [int64] vector type *)
  type t

  (** [v x y] conststructs a vector from [x] and [y] coordinates *)
  val v : int64 -> int64 -> t

  (** [x t] obtains the x coordinate of the vector [t] *)
  val x : t -> int64

  (** [y t] obtains the y coordinate of the vector [t] *)
  val y : t -> int64
end

module type Poly = sig
  (** Signature of a polygon type -- an outer path and zero or more inner paths
    (holes). To be used for construction and destruction of Clipper2 paths
    types. *)

  (** 2d vector type representing points *)
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
  (** 2d vector type representing points *)
  type v

  (** polygon type -- outer path and zero or more inner paths (holes) *)
  type poly

  (** The Clipper2 Path type (std::vector of point) *)
  type path

  (** The Clipper2 Paths type (std::vector of path) *)
  type paths

  include module type of ConfigTypes

  module Path : sig
    (** The Clipper2 path type (std::vector of point) *)

    type t = path

    (** [length t]

       Return the number of points in the path [t]. *)
    val length : t -> int

    (** [get_point t i]

       Get the point at index [i] from the path [t]. *)
    val get_point : t -> int -> v

    (** [of_list ps]

       Create a path from the list of 2d points [ps]. *)
    val of_list : v list -> t

    (** [to_list t]

       Convert the path [t] to a list of 2d points. *)
    val to_list : t -> v list

    (** [ellipse ?fn ?centre rs]

        Draw an elliptical path with with the specified xy radii [rs] centred
        on the origin, or at the point [centre] if provided. The number
        of segments can be set explicitly with [fn], otherwise the quality is
        calculated based on the dimensions. *)
    val ellipse : ?fn:int -> ?centre:v -> v -> t

    (** [translate v t]

       Translate the path [t] along the vector [v]. *)
    val translate : v -> t -> t

    (** [simplify ?closed ?eps t]

       Remove extraneous vertices from the path [t] (similar to
       {!ramer_douglas_peucker}). *)
    val simplify : ?closed:bool -> ?eps:float -> t -> t

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
    val ramer_douglas_peucker : ?eps:float -> t -> t

    (** [trim_collinear ?closed t]

          Remove collinear points (that fall on a line drawn between their
          neighbours) from the path [t]. The path is treated as [closed] by
          default. *)
    val trim_collinear : ?closed:bool -> t -> t

    (** [strip_near_equal ?closed ?eps t]

       Remove adjacent points that are less than [eps] distance apart from
       their neighbour from the paths [t]. The path is treated as [closed] by
       default. *)
    val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t

    (** [strip_duplicates ?closed ?eps t]

       Remove adjacent points that duplicate of their neighbours from the
       paths [t]. The path is treated as [closed] by default. *)
    val strip_duplicates : ?closed:bool -> t -> t

    (** [point_inside t p]

       Determine whether the point [p] is inside, outside, or on the border of
       [t]. *)
    val point_inside : t -> v -> [> `Inside | `OnBorder | `Outside ]

    (** [area t]

       Compute the signed area of the path [t]. *)
    val area : t -> float

    (** [is_positive t]

       Check if the orientation/winding of the path [t] is positive. *)
    val is_positive : t -> bool
  end

  module Paths : sig
    (** The Clipper2 paths type (std::vector of path)

          These lists of contours are not organized hierarchically (by
          parent-child / outer-hole) relationships, and may include any number
          of open paths or polygons. *)

    type t = paths

    (** [length t]

       Return the number of paths in the list of paths [t]. *)
    val length : t -> int

    (** [path_length t i]

       Return the number of points in the path at index [i] of [t]. *)
    val path_length : t -> int -> int

    (** [get_path t i]

       Get the path at index [i] from [t]. *)
    val get_path : t -> int -> path

    (** [get_point t i j]

       Get the point [j] from the path at index [i] in [t]. *)
    val get_point : t -> int -> int -> v

    (** [of_list ps]

       Create a path from the list of lists of 2d points [ps]. *)
    val of_list : v list list -> t

    (** [to_list t]

       Convert the paths [t] to a list of lists of 2d points. *)
    val to_list : t -> v list list

    (** [translate v t]

       Translate the paths [t] along the vector [v]. *)
    val translate : v -> t -> t

    (** [simplify ?closed ?eps t]

       Remove extraneous vertices from the paths [t] (similar to
       {!ramer_douglas_peucker}). *)
    val simplify : ?closed:bool -> ?eps:float -> t -> t

    (** [ramer_douglas_peucker ?eps t]

       Applies the
       {{:https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm}
       Ramer-Douglas-Peucker algorithm} to remove extraneous vertices from the
       paths [t]. Put simply, vertices will be removed if they are less than
       [eps] distance from imaginary lines passing through their adjacent vertices.

       This function is particularly useful following offsetting (ie
       inflating/shrinking paths). Offsetting often creates tiny segments that don't
       improve path quality. Further these tiny segments create angles that are
       strongly influenced by integer rounding. While these tiny segments are too
       small to be noticeable following a single offset procedure, they're likely to
       degrade the quality of subsequent offsets. And they'll also degrade
       performance. Because of this, it is strongly recommended calling this function
       after every polygon offset. *)
    val ramer_douglas_peucker : ?eps:float -> t -> t

    (** [strip_near_equal ?closed ?eps t]

       Remove adjacent points that are less than [eps] distance apart from
       their neighbour from the paths [t]. The path is treated as [closed] by
       default. *)
    val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t

    (** [strip_duplicates ?closed ?eps t]

       Remove adjacent points that duplicate of their neighbours from the
       paths [t]. The path is treated as [closed] by default. *)
    val strip_duplicates : ?closed:bool -> t -> t

    (** [area t]

       Compute the summed signed area of all paths in [t]. *)
    val area : t -> float

    (** [boolean_op ?fill_rule ~op subjects clips]

          Perform the boolean operation [op] with the specified [fill_rule]
          on the paths/polygons [subjects] with [clips] (all paths are treated as
          closed). With the exception of [`Union], operations are performed {i
          between} [subjects] and [clips], not {i within} [subjects] ({i e.g.}
          paths in [subjects] are not differenced from eachother). *)
    val boolean_op : ?fill_rule:fill_rule -> op:clip_type -> t -> t -> t

    (** [intersect ?fill_rule subjects clips]

          Intersect the polygons in [subjects] with the polygons in [clips]
          according to [fill_rule]. The result includes regions covered by both
          polygons in [subjects] and [clips]. *)
    val intersect : ?fill_rule:fill_rule -> t -> t -> t

    (** [union ?fill_rule subjects]

          Union the polygons [subjects] according to [fill_rule]. The result
          includes the regions covered by any of the polygons contained in
          [subjects]. *)
    val union : ?fill_rule:fill_rule -> t -> t

    (** [add ?fill_rule a b]

          {!union} all of the polygons in [a] and [b]. *)
    val add : ?fill_rule:fill_rule -> t -> t -> t

    (** [difference ?fill_rule subjects clips]

          Difference the polygons in [clips] from [subjects] according to
          [fill_rule]. The result includes the regions covered by the [subjects],
          but not the [clips] polygons. *)
    val difference : ?fill_rule:fill_rule -> t -> t -> t

    (** [sub a b]

           Difference the polygons in [b] from [a] (alias to {!difference}). *)
    val sub : ?fill_rule:fill_rule -> t -> t -> t

    (** [xor ?fill_rule subjects clips]

          Perform the exclusive-or boolean operation between the closed paths
          [subjects] and [clips] according to [fill_rule]. The result includes
          regions covered by the [subjects] or [clips] polygons, but not both. *)
    val xor : ?fill_rule:fill_rule -> t -> t -> t

    (** [inflate ?join_type ?end_type ~delta t]

          Offset the polygons (and/or open paths) in [t] by [delta]. If you
          intend to inflate polygons (closed paths), it's important that [end_type] is
          [`Polygon] (default if not overridden by user's [Config]). If instead you
          select one of the open path end types (e.g. [`Joined]), the polygon's {i
          outline} will be inflated
          ({{:https://github.com/AngusJohnson/Clipper2/discussions/154#discussion-4284428}
          example}).

          - with closed paths (polygons), a positive delta specifies how much outer
            polygon contours will expand and how much inner "hole" contours will contract (and
            the converse with negative deltas).
          - with open paths (polylines), including EndType.Join, delta specifies the width
            of the inflated line.
          - {b Caution:} offsetting self-intersecting polygons may produce
            unexpected results. *)
    val inflate : ?join_type:join_type -> ?end_type:end_type -> delta:float -> t -> t

    (** [of_poly p]

          Create a paths from a polygon [p]. *)
    val of_poly : poly -> t

    (** [of_polys ps]

          Create a paths from a list of polygons [ps]. *)
    val of_polys : poly list -> t

    (** [to_polys ?fill_rule t]

          Extract a list of non-overlapping polygons from the set of paths [t].
          This involves a clipper union operation tracking the parent-child
          (outline-hole) relationships of the paths, thus [fill_rule] can be
          provided to override the default rule if desired. *)
    val to_polys : ?fill_rule:fill_rule -> t -> poly list
  end
end

module type Intf = sig
  (** {1 Functor Parameters }*)

  module type VD = VD
  module type V64 = V64
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
  module MakeD' : functor (V : VD) (P : Poly with type v := V.t) (_ : Config) ->
    S with type v := V.t and type poly := P.t

  (** [MakeD (V) (C)] creates a Clipper2 module with the 2d [float] vector [V], and a user
       configuration (see {!config} for convenience constructor). Same as
       {!MakeD'}, but the polygon type is preset to [V.t list list]. *)
  module MakeD : functor (V : VD) (_ : Config) ->
    S with type v := V.t and type poly := V.t list list

  (** [MakeD' (V) (P) (C)] creates a Clipper2 module with the 2d [int64] vector
       [V], the polygon type [P] (composed of [V.t]s, used for input/output), and a
       user configuration (see {!config} for convenience constructor). *)
  module Make64' : functor (V : V64) (P : Poly with type v := V.t) (_ : Config) ->
    S with type v := V.t and type poly := P.t

  (** [MakeD (V) (C)] creates a Clipper2 module with the 2d [int64] vector [V], and a user
       configuration (see {!config} for convenience constructor). Same as
       {!MakeD'}, but the polygon type is preset to [V.t list list]. *)
  module Make64 : functor (V : V64) (_ : Config) ->
    S with type v := V.t and type poly := V.t list list
end
