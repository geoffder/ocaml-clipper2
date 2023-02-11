module type VD = sig
  type t

  val v : float -> float -> t
  val x : t -> float
  val y : t -> float
end

module type V64 = sig
  type t

  val v : int64 -> int64 -> t
  val x : t -> int64
  val y : t -> int64
end

module type Poly = sig
  type v
  type t

  val of_list : v list list -> t
end

module ConfigTypes = struct
  type clip_type =
    [ `None
    | `Intersection
    | `Union
    | `Difference
    | `Xor
    ]

  type fill_rule =
    [ `EvenOdd
    | `NonZero
    | `Positive
    | `Negative
    ]

  type join_type =
    [ `Square
    | `Round
    | `Miter
    ]

  type end_type =
    [ `Polygon
    | `Joined
    | `Butt
    | `Square
    | `Round
    ]
end

module type Config = sig
  val fill_rule : ConfigTypes.fill_rule option
  val join_type : ConfigTypes.join_type option
  val end_type : ConfigTypes.end_type option
  val precision : int option
  val eps : float option
end

module type Path = sig
  type v

  (** The Clipper2 path type (std::vector of points) *)
  type t

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

       *)
  val simplify : ?closed:bool -> ?eps:float -> t -> t

  (** [ramer_douglas_peucker ?eps t]

       *)
  val ramer_douglas_peucker : ?eps:float -> t -> t

  (** [strip_near_equal ?closed ?eps t]

       *)
  val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t

  (** [strip_duplicates ?closed ?eps t]

       *)
  val strip_duplicates : ?closed:bool -> t -> t

  (** [point_inside t p]

       *)
  val point_inside : t -> v -> [> `Inside | `OnBorder | `Outside ]

  (** [area t]

       Compute the signed area of the path [t]. *)
  val area : t -> float

  (** [is_positive t]

       Check if the orientation/winding of the path [t] is positive. *)
  val is_positive : t -> bool
end

module type Paths = sig
  type path
  type v

  (** The Clipper2 paths type (std::vector of path) *)
  type t

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

       *)
  val simplify : ?closed:bool -> ?eps:float -> t -> t

  (** [ramer_douglas_peucker ?eps t]

       *)
  val ramer_douglas_peucker : ?eps:float -> t -> t

  (** [strip_near_equal ?closed ?eps t]

       *)
  val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t

  (** [strip_duplicates ?closed ?eps t]

       *)
  val strip_duplicates : ?closed:bool -> t -> t

  (** [area t]

       Compute the summed signed area of all paths in [t]. *)
  val area : t -> float
end

module type SD = sig
  type v
  type poly

  include module type of ConfigTypes

  module Path : sig
    include Path with type v := v

    val trim_collinear : ?closed:bool -> ?precision:int -> t -> t
  end

  module Paths : sig
    include Paths with type v := v and type path := Path.t

    val boolean_op
      :  ?fill_rule:ConfigTypes.fill_rule
      -> ?precision:int
      -> op:ConfigTypes.clip_type
      -> t
      -> t
      -> t

    val intersect : ?fill_rule:ConfigTypes.fill_rule -> ?precision:int -> t -> t -> t
    val union : ?fill_rule:ConfigTypes.fill_rule -> ?precision:int -> t -> t
    val difference : ?fill_rule:ConfigTypes.fill_rule -> ?precision:int -> t -> t -> t
    val xor : ?fill_rule:ConfigTypes.fill_rule -> ?precision:int -> t -> t -> t

    val inflate
      :  ?join_type:ConfigTypes.join_type
      -> ?end_type:ConfigTypes.end_type
      -> ?miter_limit:float
      -> ?precision:int
      -> delta:float
      -> t
      -> t

    val to_polys : ?fill_rule:ConfigTypes.fill_rule -> ?precision:int -> t -> poly list
  end
end

module type S64 = sig
  type v
  type poly

  include module type of ConfigTypes

  module Path : sig
    include Path with type v := v

    val trim_collinear : ?closed:bool -> t -> t
  end

  module Paths : sig
    include Paths with type v := v and type path := Path.t

    val boolean_op
      :  ?fill_rule:ConfigTypes.fill_rule
      -> op:ConfigTypes.clip_type
      -> t
      -> t
      -> t

    val intersect : ?fill_rule:ConfigTypes.fill_rule -> t -> t -> t
    val union : ?fill_rule:ConfigTypes.fill_rule -> t -> t
    val difference : ?fill_rule:ConfigTypes.fill_rule -> t -> t -> t
    val xor : ?fill_rule:ConfigTypes.fill_rule -> t -> t -> t

    val inflate
      :  ?join_type:ConfigTypes.join_type
      -> ?end_type:ConfigTypes.end_type
      -> ?miter_limit:float
      -> delta:float
      -> t
      -> t

    val to_polys : ?fill_rule:ConfigTypes.fill_rule -> t -> poly list
  end
end

module type Intf = sig
  module type VD = VD
  module type V64 = V64
  module type Poly = Poly
  module type Config = Config

  val config
    :  ?fill_rule:ConfigTypes.fill_rule
    -> ?join_type:ConfigTypes.join_type
    -> ?end_type:ConfigTypes.end_type
    -> ?precision:int
    -> ?eps:float
    -> unit
    -> (module Config)

  module type SD = SD
  module type S64 = S64

  module MakeD' : functor (V : VD) (P : Poly with type v := V.t) (_ : Config) ->
    SD with type v := V.t and type poly := P.t

  module MakeD : functor (V : VD) -> SD with type v := V.t and type poly := V.t list list

  module Make64' : functor (V : V64) (P : Poly with type v := V.t) (_ : Config) ->
    S64 with type v := V.t and type poly := P.t

  module Make64 : functor (V : V64) ->
    S64 with type v := V.t and type poly := V.t list list
end
