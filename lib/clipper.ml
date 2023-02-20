include Clipper_intf
include ConfigTypes

let config ?fill_rule ?join_type ?end_type ?precision ?eps () =
  ( module struct
    let fill_rule = fill_rule
    let join_type = join_type
    let end_type = end_type
    let precision = precision
    let eps = eps
  end : Config )

module MakeD' (V : V with type n := float) (P : Poly with type v := V.t) (Conf : Config) =
struct
  type cpath = C.Types.PathD.t Ctypes_static.ptr
  type cpaths = C.Types.PathsD.t Ctypes_static.ptr

  type ('cpp, 'list) t =
    | Path : cpath -> ([ `Path ], V.t list) t
    | Paths : cpaths -> ([ `Paths ], V.t list list) t

  include ConfigTypes

  let fill_rule = Option.value ~default:`NonZero Conf.fill_rule
  let join_type = Option.value ~default:`Round Conf.join_type
  let end_type = Option.value ~default:`Polygon Conf.end_type

  let precision =
    match Conf.precision with
    | None -> 2
    | Some digits when digits >= 0 && digits <= 8 -> digits
    | _ -> invalid_arg "Precision must be between 0 and 8."

  let eps =
    match Conf.eps with
    | Some eps -> eps
    | None -> 1. /. Float.pow 10. (Float.of_int precision)

  module Point = struct
    include PointD

    let[@inline] of_v v = make (V.x v) (V.y v)
    let[@inline] to_v t = V.v (x t) (y t)
  end

  module Rect = struct
    include RectD

    let make ~l ~t ~r ~b =
      let buf, rect = alloc () in
      let _ = C.Funcs.rectd buf l t r b in
      rect

    let of_pts a b =
      let buf, t = alloc ()
      and left = Float.min (V.x a) (V.x b)
      and right = Float.max (V.x a) (V.x b)
      and bottom = Float.min (V.y a) (V.y b)
      and top = Float.max (V.y a) (V.y b) in
      let _ = C.Funcs.rectd buf left top right bottom in
      t

    let width t = C.Funcs.rectd_width t
    let height t = C.Funcs.rectd_height t
    let midpoint t = Point.to_v @@ C.Funcs.rectd_midpoint t

    let scale s t =
      let buf, scaled = alloc () in
      let _ = C.Funcs.rectd_scale buf t s in
      scaled

    let as_path t =
      let buf, p = PathD.alloc () in
      let _ = C.Funcs.rectd_as_path buf t in
      p

    let contains_pt t p = C.Funcs.rectd_contains_pt t (Point.of_v p)
    let contains_rect a b = C.Funcs.rectd_contains_rect a b
    let is_empty t = C.Funcs.rectd_is_empty t
    let intersects a b = C.Funcs.rectd_intersects a b
  end

  module Path0 = struct
    include PathD

    let[@inline] get_point t i = Point.to_v @@ C.Funcs.pathd_get_point t i
    let[@inline] add_point t v = C.Funcs.pathd_add_point t (Point.of_v v)

    let of_list vs =
      let t = make () in
      List.iter (add_point t) vs;
      t

    let to_list t = List.init (length t) (get_point t)
  end

  let[@inline] ( .%() ) (Path p) i = Point.to_v @@ C.Funcs.pathd_get_point p i
  let[@inline] ( .%{} ) (Paths ps) (i, j) = Point.to_v @@ C.Funcs.pathsd_get_point ps i j
  let[@inline] of_list vs = Path (Path0.of_list vs)

  let of_lists paths =
    let c = PathsD.make () in
    List.iter (fun p -> PathsD.add_path c (Path0.of_list p)) paths;
    Paths c

  let to_list : type c l. (c, l) t -> l = function
    | Path p -> Path0.to_list p
    | Paths ps ->
      List.init (PathsD.length ps) (fun i ->
        let len = Conv.size_to_int @@ C.Funcs.pathsd_path_length t i in
        List.init len (fun j -> Point.to_v @@ C.Funcs.pathsd_get_point ps i j) )

  let ellipse ?(fn = 0) ?centre wh =
    let centre =
      match centre with
      | Some c -> Point.of_v c
      | None -> Point.make 0. 0.
    and buf, p = PathD.alloc () in
    let _ = C.Funcs.pathd_ellipse buf centre (V.x wh *. 0.5) (V.y wh *. 0.5) fn in
    Path p

  let translate (type c l) v (t : (c, l) t) : (c, l) t =
    match t with
    | Path p ->
      let buf, translated = PathD.alloc () in
      let _ = C.Funcs.pathd_translate buf p (V.x v) (V.y v) in
      Path translated
    | Paths ps ->
      let buf, translated = PathsD.alloc () in
      let _ = C.Funcs.pathsd_translate buf ps (V.x v) (V.y v) in
      Paths translated

  let boolean_op
    (type c l)
    ?(fill_rule = fill_rule)
    ~op
    (subjects : (c, l) t)
    (clips : (c, l) t list)
    =
    let buf, t = PathsD.alloc ()
    and op = ClipType.make op
    and fill_rule = FillRule.make fill_rule
    and cs = PathsD.make () in
    let ss =
      match subjects with
      | Path s ->
        let ss = PathsD.make () in
        List.iter (fun (Path p) -> PathsD.add_path cs p) clips;
        PathsD.add_path ss s;
        ss
      | Paths ss ->
        List.iter
          (fun (Paths ps) ->
            let len = PathsD.length ps in
            for i = 0 to len - 1 do
              PathsD.add_path cs (PathsD.get_path ps i)
            done )
          clips;
        ss
    in
    let _ = C.Funcs.pathsd_boolean_op buf op fill_rule ss cs precision in
    Paths t

  let boolean_op_tree
    (type c l)
    ?(fill_rule = fill_rule)
    ~op
    (subjects : (c, l) t)
    (clips : (c, l) t list)
    =
    let tree = PolyTreeD.make ()
    and op = ClipType.make op
    and fill_rule = FillRule.make fill_rule
    and cs = PathsD.make () in
    let ss =
      match subjects with
      | Path s ->
        let ss = PathsD.make () in
        List.iter (fun (Path p) -> PathsD.add_path cs p) clips;
        PathsD.add_path ss s;
        ss
      | Paths ss ->
        List.iter
          (fun (Paths ps) ->
            let len = PathsD.length ps in
            for i = 0 to len - 1 do
              PathsD.add_path cs (PathsD.get_path ps i)
            done )
          clips;
        ss
    in
    let _ = C.Funcs.pathsd_boolean_op_tree op fill_rule ss cs tree precision in
    tree

  let intersect ?fill_rule subjects clips =
    boolean_op ?fill_rule ~op:`Intersection subjects clips

  let union ?fill_rule = function
    | [] -> Paths (PathsD.make ())
    | hd :: tl -> boolean_op ?fill_rule ~op:`Union hd tl

  let add ?fill_rule a b = boolean_op ?fill_rule ~op:`Union a [ b ]

  let difference ?fill_rule subjects clips =
    boolean_op ?fill_rule ~op:`Difference subjects clips

  let sub ?fill_rule a b = difference ?fill_rule a [ b ]
  let xor ?fill_rule subjects clips = boolean_op ?fill_rule ~op:`Xor subjects clips

  let bounds (type c l) (t : (c, l) t) =
    let buf, rect = RectD.alloc () in
    let _ =
      match t with
      | Path p -> C.Funcs.pathd_bounds buf p
      | Paths ps -> C.Funcs.pathsd_bounds buf ps
    in
    rect

  let rect_clip (type c l) ?(closed = true) rect (t : (c, l) t) =
    let buf, paths = PathsD.alloc () in
    let _ =
      match t, closed with
      | Path p, true -> C.Funcs.pathd_rect_clip buf rect p precision
      | Path p, false -> C.Funcs.pathd_rect_clip_line buf rect p precision
      | Paths ps, true -> C.Funcs.pathsd_rect_clip buf rect ps precision
      | Paths ps, false -> C.Funcs.pathsd_rect_clip_lines buf rect ps precision
    in
    paths

  let inflate
    (type c l)
    ?(join_type = join_type)
    ?(end_type = end_type)
    ~delta
    (t : (c, l) t)
    =
    let buf, inflated = PathsD.alloc ()
    and join_type, miter_limit = JoinType.make join_type
    and end_type = EndType.make end_type in
    let ps =
      match t with
      | Path p ->
        let ps = PathsD.make () in
        PathsD.add_path ps p;
        ps
      | Paths ps -> ps
    in
    let _ =
      C.Funcs.pathsd_inflate buf ps delta join_type end_type miter_limit precision
    in
    Paths inflated

  let strip_near_equal (type c l) ?(closed = true) ?(eps = eps) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p ->
      let buf, stripped = PathD.alloc () in
      let _ = C.Funcs.pathd_strip_near_equal buf p eps closed in
      Path stripped
    | Paths ps ->
      let buf, stripped = PathsD.alloc () in
      let _ = C.Funcs.pathsd_strip_near_equal buf ps eps closed in
      Paths stripped

  let strip_duplicates (type c l) ?(closed = true) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p ->
      let buf, stripped = PathD.alloc () in
      let _ = C.Funcs.pathd_strip_duplicates buf p closed in
      Path stripped
    | Paths ps ->
      let buf, stripped = PathsD.alloc () in
      let _ = C.Funcs.pathsd_strip_duplicates buf ps closed in
      Paths stripped

  let simplify (type c l) ?(closed = true) ?(eps = eps) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p ->
      let buf, simplified = PathD.alloc () in
      let _ = C.Funcs.pathd_simplify buf p eps closed in
      Path simplified
    | Paths ps ->
      let buf, simplified = PathsD.alloc () in
      let _ = C.Funcs.pathsd_simplify buf ps eps closed in
      Paths simplified

  let ramer_douglas_peucker (type c l) ?(eps = eps) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p ->
      let buf, rdp = PathD.alloc () in
      let _ = C.Funcs.pathd_ramer_douglas_peucker buf p eps in
      Path rdp
    | Paths ps ->
      let buf, rdp = PathsD.alloc () in
      let _ = C.Funcs.pathsd_ramer_douglas_peucker buf ps eps in
      Paths rdp

  let area : type c l. (c, l) t -> float = function
    | Path p -> C.Funcs.pathd_area p
    | Paths ps -> C.Funcs.pathsd_area ps

  let point_inside (Path path) p =
    match C.Funcs.pointd_in_path path (Point.of_v p) with
    | IsOutside -> `Outside
    | IsInside -> `Inside
    | IsOn -> `OnBorder

  let is_positive (Path p) = C.Funcs.pathd_is_positive p
  let of_poly p = of_lists @@ P.to_list p

  let[@inline] of_polys ps =
    of_lists @@ List.fold_left (fun acc p -> List.rev_append (P.to_list p) acc) [] ps

  let to_poly (Path p) = P.of_list [ Path0.to_list p ]

  let to_polys ?(fill_rule = fill_rule) t =
    let tree = boolean_op_tree ~fill_rule ~op:`Union t [] in
    let polys = PolyTreeD.decompose Path0.to_list tree in
    List.map P.of_list polys

  (* let minkowski_sum ?(closed = true) ~pattern (Path p) = *)
  (*   let buf, summed = PathD.alloc () in *)
  (*   let _ = C.Funcs.pathd_minkowski_sum buf pattern p closed precision in *)
  (*   Path summed *)

  (* let minkowski_diff ?(closed = true) ~pattern (Path p) = *)
  (*   let buf, diffed = PathD.alloc () in *)
  (*   let _ = C.Funcs.pathd_minkowski_diff buf pattern p closed precision in *)
  (*   Path diffed *)

  let minkowski
    (type c l)
    ?(closed = true)
    ?(fill_rule = fill_rule)
    ~pattern
    ~f
    (t : (c, l) t)
    : (c, l) t
    =
    match t with
    | Path p ->
      let buf, summed = PathD.alloc () in
      let _ = f buf pattern p closed precision in
      Path summed
    | Paths ps ->
      let summed = PathsD.make () in
      let len = PathsD.length ps in
      for i = 0 to len - 1 do
        let buf, s = PathD.alloc () in
        let p = PathsD.get_path summed i in
        let _ = f buf pattern p closed precision in
        PathsD.add_path summed s
      done;
      let buf, unioned = PathsD.alloc ()
      and fill_rule = FillRule.make fill_rule in
      let _ = C.Funcs.pathsd_union buf summed (PathsD.make ()) fill_rule precision in
      Paths unioned

  let minkowski_sum ?closed ?fill_rule ~pattern t =
    minkowski ?closed ?fill_rule ~pattern ~f:C.Funcs.pathd_minkowski_sum t

  let minkowski_diff ?closed ?fill_rule ~pattern t =
    minkowski ?closed ?fill_rule ~pattern ~f:C.Funcs.pathd_minkowski_diff t
end

module MakeD (V : V with type n := float) (Conf : Config) =
  MakeD'
    (V)
    (struct
      type t = V.t list list

      let to_list = Fun.id
      let of_list = Fun.id
    end)
    (Conf)

module Make64' (V : V with type n := int64) (P : Poly with type v := V.t) (Conf : Config) =
struct
  type path = C.Types.Path64.t Ctypes_static.ptr
  type paths = C.Types.Paths64.t Ctypes_static.ptr

  include ConfigTypes

  let fill_rule = Option.value ~default:`NonZero Conf.fill_rule
  let join_type = Option.value ~default:`Round Conf.join_type
  let end_type = Option.value ~default:`Polygon Conf.end_type
  let eps = Option.value ~default:1. Conf.eps

  module Point = struct
    include Point64

    let[@inline] of_v v = make (V.x v) (V.y v)
    let[@inline] to_v t = V.v (x t) (y t)
  end

  module Rect = struct
    include Rect64

    let make ~l ~t ~r ~b =
      let buf, rect = alloc () in
      let _ = C.Funcs.rect64 buf l t r b in
      rect

    let of_pts a b =
      let buf, t = alloc ()
      and left = Int64.min (V.x a) (V.x b)
      and right = Int64.max (V.x a) (V.x b)
      and bottom = Int64.min (V.y a) (V.y b)
      and top = Int64.max (V.y a) (V.y b) in
      let _ = C.Funcs.rect64 buf left top right bottom in
      t

    let width t = C.Funcs.rect64_width t
    let height t = C.Funcs.rect64_height t
    let midpoint t = Point.to_v @@ C.Funcs.rect64_midpoint t

    let scale s t =
      let buf, scaled = alloc () in
      let _ = C.Funcs.rect64_scale buf t s in
      scaled

    let as_path t =
      let buf, p = Path64.alloc () in
      let _ = C.Funcs.rect64_as_path buf t in
      p

    let contains_pt t p = C.Funcs.rect64_contains_pt t (Point.of_v p)
    let contains_rect a b = C.Funcs.rect64_contains_rect a b
    let is_empty t = C.Funcs.rect64_is_empty t
    let intersects a b = C.Funcs.rect64_intersects a b
  end

  module Path0 = struct
    include Path64

    let get_point t i = Point.to_v @@ C.Funcs.path64_get_point t i
    let add_point t v = C.Funcs.path64_add_point t (Point.of_v v)

    let of_list ps =
      let t = make () in
      List.iter (add_point t) ps;
      t

    let to_list t =
      List.init (length t) (fun i -> Point.to_v @@ C.Funcs.path64_get_point t i)
  end

  module Paths = struct
    include Paths64

    let translate v t =
      let buf, translated = alloc () in
      let _ = C.Funcs.paths64_translate buf t (V.x v) (V.y v) in
      translated

    let[@inline] get_point t i j = Point.to_v @@ C.Funcs.paths64_get_point t i j

    let of_list paths =
      let t = make () in
      List.iter (fun p -> add_path t (Path0.of_list p)) paths;
      t

    let to_list t =
      List.init (length t) (fun i ->
        let len = Conv.size_to_int @@ C.Funcs.paths64_path_length t i in
        List.init len (fun j -> get_point t i j) )

    let boolean_op ?(fill_rule = fill_rule) ~op subjects clips =
      let buf, t = alloc ()
      and op = ClipType.make op
      and fill_rule = FillRule.make fill_rule in
      let _ = C.Funcs.paths64_boolean_op buf op fill_rule subjects clips in
      t

    let boolean_op_tree ?(fill_rule = fill_rule) ~op subjects clips =
      let tree = PolyTree64.make ()
      and op = ClipType.make op
      and fill_rule = FillRule.make fill_rule in
      let _ = C.Funcs.paths64_boolean_op_tree op fill_rule subjects clips tree in
      tree

    let intersect ?(fill_rule = fill_rule) subjects clips =
      let buf, t = alloc ()
      and fill_rule = FillRule.make fill_rule in
      let _ = C.Funcs.paths64_intersect buf subjects clips fill_rule in
      t

    let add ?(fill_rule = fill_rule) subjects clips =
      let buf, t = alloc ()
      and fill_rule = FillRule.make fill_rule in
      let _ = C.Funcs.paths64_union buf subjects clips fill_rule in
      t

    let[@inline] union ?fill_rule subjects = add ?fill_rule subjects (make ())

    let difference ?(fill_rule = fill_rule) subjects clips =
      let buf, t = alloc ()
      and fill_rule = FillRule.make fill_rule in
      let _ = C.Funcs.paths64_difference buf subjects clips fill_rule in
      t

    let[@inline] sub ?fill_rule subjects clips = difference ?fill_rule subjects clips

    let xor ?(fill_rule = fill_rule) subjects clips =
      let buf, t = alloc ()
      and fill_rule = FillRule.make fill_rule in
      let _ = C.Funcs.paths64_xor buf subjects clips fill_rule in
      t

    let bounds t =
      let buf, rect = Rect64.alloc () in
      let _ = C.Funcs.paths64_bounds buf t in
      rect

    let rect_clip ?(closed = true) rect t =
      let buf, paths = Paths64.alloc () in
      let _ =
        if closed
        then C.Funcs.paths64_rect_clip buf rect t
        else C.Funcs.paths64_rect_clip_lines buf rect t
      in
      paths

    let inflate ?(join_type = join_type) ?(end_type = end_type) ~delta t =
      let buf, inflated = alloc ()
      and join_type, miter_limit = JoinType.make join_type
      and end_type = EndType.make end_type in
      let _ = C.Funcs.paths64_inflate buf t delta join_type end_type miter_limit in
      inflated

    let strip_near_equal ?(closed = true) ?(eps = eps) t =
      let buf, stripped = alloc () in
      let _ = C.Funcs.paths64_strip_near_equal buf t eps closed in
      stripped

    let strip_duplicates ?(closed = true) t =
      let buf, stripped = alloc () in
      let _ = C.Funcs.paths64_strip_duplicates buf t closed in
      stripped

    let simplify ?(closed = true) ?(eps = eps) t =
      let buf, simplified = alloc () in
      let _ = C.Funcs.paths64_simplify buf t eps (not closed) in
      simplified

    let ramer_douglas_peucker ?(eps = eps) t =
      let buf, rdp = alloc () in
      let _ = C.Funcs.paths64_ramer_douglas_peucker buf t eps in
      rdp

    let area t = C.Funcs.paths64_area t
    let[@inline] of_poly p = of_list @@ P.to_list p

    let[@inline] of_polys ps =
      of_list @@ List.fold_left (fun acc p -> List.rev_append (P.to_list p) acc) [] ps

    let to_polys ?(fill_rule = fill_rule) t =
      let tree = boolean_op_tree ~fill_rule ~op:`Union t (make ()) in
      let polys = PolyTree64.decompose Path0.to_list tree in
      List.map P.of_list polys
  end

  module Path = struct
    include Path0

    let ellipse ?(fn = 0) ?centre wh =
      let centre =
        match centre with
        | Some c -> Point.of_v c
        | None -> Point.make (Int64.of_int 0) (Int64.of_int 0)
      and buf, t = alloc ()
      and rx = (Int64.to_float @@ V.x wh) *. 0.5
      and ry = (Int64.to_float @@ V.y wh) *. 0.5 in
      let _ = C.Funcs.path64_ellipse buf centre rx ry fn in
      t

    let translate v t =
      let buf, translated = alloc () in
      let _ = C.Funcs.path64_translate buf t (V.x v) (V.y v) in
      translated

    let boolean_op ?fill_rule ~op a b =
      let subjects = Paths64.make ()
      and clips = Paths64.make () in
      Paths64.add_path subjects a;
      Paths64.add_path clips b;
      Paths.boolean_op ?fill_rule ~op subjects clips

    let intersect ?fill_rule a b =
      let subjects = Paths64.make ()
      and clips = Paths64.make () in
      Paths64.add_path subjects a;
      Paths64.add_path clips b;
      Paths.intersect ?fill_rule subjects clips

    let add ?fill_rule a b =
      let subjects = Paths64.make () in
      Paths64.add_path subjects a;
      Paths64.add_path subjects b;
      Paths.union ?fill_rule subjects

    let union ?fill_rule ts =
      let subjects = Paths64.make () in
      List.iter (Paths64.add_path subjects) ts;
      Paths.union ?fill_rule subjects

    let difference ?fill_rule a b =
      let subjects = Paths64.make ()
      and clips = Paths64.make () in
      Paths64.add_path subjects a;
      Paths64.add_path clips b;
      Paths.difference ?fill_rule subjects clips

    let[@inline] sub ?fill_rule a b = difference ?fill_rule a b

    let xor ?fill_rule a b =
      let subjects = Paths64.make ()
      and clips = Paths64.make () in
      Paths64.add_path subjects a;
      Paths64.add_path clips b;
      Paths.xor ?fill_rule subjects clips

    let bounds t =
      let buf, rect = Rect64.alloc () in
      let _ = C.Funcs.path64_bounds buf t in
      rect

    let rect_clip ?(closed = true) rect t =
      let buf, paths = Paths64.alloc () in
      let _ =
        if closed
        then C.Funcs.path64_rect_clip buf rect t
        else C.Funcs.path64_rect_clip_line buf rect t
      in
      paths

    let inflate ?join_type ?end_type ~delta t =
      let subjects = Paths64.make () in
      Paths64.add_path subjects t;
      Paths.inflate ?join_type ?end_type ~delta subjects

    let minkowski_sum ?(closed = true) ~pattern t =
      let buf, summed = alloc () in
      let _ = C.Funcs.path64_minkowski_sum buf pattern t closed in
      summed

    let minkowski_diff ?(closed = true) ~pattern t =
      let buf, diffed = alloc () in
      let _ = C.Funcs.path64_minkowski_diff buf pattern t closed in
      diffed

    let trim_collinear ?(closed = true) t =
      let buf, trimmed = alloc () in
      let _ = C.Funcs.path64_trim_collinear buf t (not closed) in
      trimmed

    let simplify ?(closed = true) ?(eps = eps) t =
      let buf, simplified = alloc () in
      let _ = C.Funcs.path64_simplify buf t eps (not closed) in
      simplified

    let ramer_douglas_peucker ?(eps = eps) t =
      let buf, rdp = alloc () in
      let _ = C.Funcs.path64_ramer_douglas_peucker buf t eps in
      rdp

    let strip_near_equal ?(closed = true) ?(eps = eps) t =
      let buf, stripped = alloc () in
      let _ = C.Funcs.path64_strip_near_equal buf t eps closed in
      stripped

    let strip_duplicates ?(closed = true) t =
      let buf, stripped = alloc () in
      let _ = C.Funcs.path64_strip_duplicates buf t closed in
      stripped

    let point_inside t p =
      match C.Funcs.point64_in_path t (Point.of_v p) with
      | IsOutside -> `Outside
      | IsInside -> `Inside
      | IsOn -> `OnBorder

    let area t = C.Funcs.path64_area t
    let is_positive t = C.Funcs.path64_is_positive t
  end
end

module Make64 (V : V with type n := int64) (Conf : Config) =
  Make64'
    (V)
    (struct
      type t = V.t list list

      let to_list = Fun.id
      let of_list = Fun.id
    end)
    (Conf)
