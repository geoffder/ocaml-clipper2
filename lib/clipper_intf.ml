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
  type t

  val of_list : v list -> t
  val to_list : t -> v list
  val ellipse : ?fn:int -> ?centre:v -> float -> float -> t
  val translate : v -> t -> t
  val simplify : ?closed:bool -> ?eps:float -> t -> t
  val ramer_douglas_peucker : ?eps:float -> t -> t
  val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t
  val strip_duplicates : ?closed:bool -> t -> t
  val point_inside : t -> v -> [> `Inside | `OnBorder | `Outside ]
  val area : t -> float
  val is_positive : t -> bool
end

module type Paths = sig
  type path
  type v
  type t

  val length : t -> int
  val path_length : t -> int -> int
  val get_path : t -> int -> path
  val get_point : t -> int -> int -> v
  val of_list : v list list -> t
  val to_list : t -> v list list
  val translate : v -> t -> t
  val strip_near_equal : ?closed:bool -> ?eps:float -> t -> t
  val strip_duplicates : ?closed:bool -> t -> t
  val simplify : ?closed:bool -> ?eps:float -> t -> t
  val ramer_douglas_peucker : ?eps:float -> t -> t
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
