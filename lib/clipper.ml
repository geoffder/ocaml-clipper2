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

module MakeD'
  (V : V with type n := float)
  (Ctr : Contour with type v := V.t)
  (P : Poly with type v := V.t)
  (Conf : Config) =
struct
  type cpath = C.Types.PathD.t Ctypes_static.ptr
  type cpaths = C.Types.PathsD.t Ctypes_static.ptr

  type ('cpp, 'list) t =
    | Path : cpath -> ([ `Path ], Ctr.t) t
    | Paths : cpaths -> ([ `Paths ], Ctr.t list) t

  type path = ([ `Path ], Ctr.t) t
  type paths = ([ `Paths ], Ctr.t list) t

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
      Path p

    let contains_pt t p = C.Funcs.rectd_contains_pt t (Point.of_v p)
    let contains_rect a b = C.Funcs.rectd_contains_rect a b
    let is_empty t = C.Funcs.rectd_is_empty t
    let intersects a b = C.Funcs.rectd_intersects a b
  end

  let n_pts : type c l. (c, l) t -> int = function
    | Path p -> PathD.length p
    | Paths ps ->
      let n = ref 0 in
      let len = PathsD.length ps in
      for i = 0 to len - 1 do
        n := !n + PathD.length (PathsD.get_path ps i)
      done;
      !n

  let n_pts_sub (Paths ps) i = PathsD.path_length ps i

  let n_paths : type c l. (c, l) t -> int = function
    | Path _ -> 1
    | Paths ps -> PathsD.length ps

  let[@inline] cpath_get_point t i = Point.to_v @@ C.Funcs.pathd_get_point t i
  let cpath_to_list t = List.init (PathD.length t) (cpath_get_point t)

  let cpath_of_list vs =
    let t = PathD.make () in
    List.iter (fun v -> C.Funcs.pathd_add_point t (Point.of_v v)) vs;
    t

  let cpath_to_contour t = Ctr.of_seq @@ Seq.init (PathD.length t) (cpath_get_point t)

  let cpath_of_contour vs =
    let t = PathD.make () in
    Seq.iter (fun v -> C.Funcs.pathd_add_point t (Point.of_v v)) (Ctr.to_seq vs);
    t

  let path_pt (Path p) i =
    if i >= 0 && i < PathD.length p
    then cpath_get_point p i
    else invalid_arg "path_pt: out of bounds access"

  let paths_pt (Paths ps) i j =
    if i >= 0 && i < PathsD.length ps && j >= 0 && j < PathsD.path_length ps i
    then Point.to_v @@ C.Funcs.pathsd_get_point ps i j
    else invalid_arg "paths_pt: out of bounds access"

  let[@inline] ( .%() ) p i = path_pt p i
  let[@inline] ( .%{} ) ps (i, j) = paths_pt ps i j
  let[@inline] path vs = Path (cpath_of_contour vs)

  let paths ps =
    let c = PathsD.make () in
    List.iter (fun p -> PathsD.add_path c (cpath_of_contour p)) ps;
    Paths c

  let contour : type c l. (c, l) t -> l = function
    | Path p -> cpath_to_contour p
    | Paths ps ->
      List.init (PathsD.length ps) (fun i ->
        let len = Conv.size_to_int @@ C.Funcs.pathsd_path_length ps i in
        Ctr.of_seq
        @@ Seq.init len (fun j -> Point.to_v @@ C.Funcs.pathsd_get_point ps i j) )

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

  let intersect ?fill_rule = function
    | [] -> Paths (PathsD.make ())
    | hd :: tl -> boolean_op ?fill_rule ~op:`Intersection hd tl

  let union ?fill_rule = function
    | [] -> Paths (PathsD.make ())
    | hd :: tl -> boolean_op ?fill_rule ~op:`Union hd tl

  let add ?fill_rule a b = boolean_op ?fill_rule ~op:`Union a [ b ]

  let difference ?fill_rule subjects clips =
    boolean_op ?fill_rule ~op:`Difference subjects clips

  let sub ?fill_rule a b = difference ?fill_rule a [ b ]

  let xor ?fill_rule = function
    | [] -> Paths (PathsD.make ())
    | hd :: tl -> boolean_op ?fill_rule ~op:`Xor hd tl

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
    Paths paths

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

  let trim_collinear (type c l) ?(closed = true) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p ->
      let buf, trimmed = PathD.alloc () in
      let _ = C.Funcs.pathd_trim_collinear buf p closed precision in
      Path trimmed
    | Paths ps ->
      let trimmed = PathsD.make ()
      and len = PathsD.length ps in
      (* TODO: should make a function on the C side to cut down on copying? *)
      for i = 0 to len - 1 do
        let buf, td = PathD.alloc ()
        and p = PathsD.get_path ps i in
        let _ = C.Funcs.pathd_trim_collinear buf p closed precision in
        PathsD.add_path trimmed td
      done;
      Paths trimmed

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

  let of_poly p =
    let c = PathsD.make () in
    List.iter (fun p -> PathsD.add_path c (cpath_of_list p)) (P.to_list p);
    Paths c

  let[@inline] of_polys ps =
    let c = PathsD.make () in
    List.fold_left (fun acc p -> List.rev_append (P.to_list p) acc) [] ps
    |> List.iter (fun p -> PathsD.add_path c (cpath_of_list p));
    Paths c

  let to_poly (Path p) = P.of_list [ cpath_to_list p ]

  let to_polys ?(fill_rule = fill_rule) t =
    let tree = boolean_op_tree ~fill_rule ~op:`Union t [] in
    let polys = PolyTreeD.decompose cpath_to_list tree in
    List.map P.of_list polys

  let minkowski
    (type c l)
    ?(closed = true)
    ?(fill_rule = fill_rule)
    ~pattern:(Path pat)
    ~op
    (t : (c, l) t)
    : paths
    =
    let buf, mink = PathsD.alloc ()
    and fr = FillRule.make fill_rule in
    let _ =
      match t, op with
      | Path p, `Sum -> C.Funcs.pathd_minkowski_sum buf pat p closed precision
      | Paths ps, `Sum -> C.Funcs.pathsd_minkowski_sum buf pat ps closed precision fr
      | Path p, `Diff -> C.Funcs.pathd_minkowski_diff buf pat p closed precision
      | Paths ps, `Diff -> C.Funcs.pathsd_minkowski_diff buf pat ps closed precision fr
    in
    Paths mink

  let minkowski_sum ?closed ?fill_rule ~pattern t =
    minkowski ?closed ?fill_rule ~pattern ~op:`Sum t

  let minkowski_diff ?closed ?fill_rule ~pattern t =
    minkowski ?closed ?fill_rule ~pattern ~op:`Diff t
end

module MakeD (V : V with type n := float) (Conf : Config) =
  MakeD'
    (V)
    (struct
      type t = V.t list

      let of_seq s = List.of_seq s
      let to_seq l = List.to_seq l
    end)
    (struct
      type t = V.t list list

      let to_list = Fun.id
      let of_list = Fun.id
    end)
    (Conf)

module Make64'
  (V : V with type n := int64)
  (Ctr : Contour with type v := V.t)
  (P : Poly with type v := V.t)
  (Conf : Config) =
struct
  type cpath = C.Types.Path64.t Ctypes_static.ptr
  type cpaths = C.Types.Paths64.t Ctypes_static.ptr

  type ('cpp, 'list) t =
    | Path : cpath -> ([ `Path ], Ctr.t) t
    | Paths : cpaths -> ([ `Paths ], Ctr.t list) t

  type path = ([ `Path ], Ctr.t) t
  type paths = ([ `Paths ], Ctr.t list) t

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
      Path p

    let contains_pt t p = C.Funcs.rect64_contains_pt t (Point.of_v p)
    let contains_rect a b = C.Funcs.rect64_contains_rect a b
    let is_empty t = C.Funcs.rect64_is_empty t
    let intersects a b = C.Funcs.rect64_intersects a b
  end

  let n_pts : type c l. (c, l) t -> int = function
    | Path p -> Path64.length p
    | Paths ps ->
      let n = ref 0 in
      let len = Paths64.length ps in
      for i = 0 to len - 1 do
        n := !n + Path64.length (Paths64.get_path ps i)
      done;
      !n

  let n_pts_sub (Paths ps) i = Paths64.path_length ps i

  let n_paths : type c l. (c, l) t -> int = function
    | Path _ -> 1
    | Paths ps -> Paths64.length ps

  let[@inline] cpath_get_point t i = Point.to_v @@ C.Funcs.path64_get_point t i
  let cpath_to_list t = List.init (Path64.length t) (cpath_get_point t)

  let cpath_of_list vs =
    let t = Path64.make () in
    List.iter (fun v -> C.Funcs.path64_add_point t (Point.of_v v)) vs;
    t

  let cpath_to_contour t = Ctr.of_seq @@ Seq.init (Path64.length t) (cpath_get_point t)

  let cpath_of_contour vs =
    let t = Path64.make () in
    Seq.iter (fun v -> C.Funcs.path64_add_point t (Point.of_v v)) (Ctr.to_seq vs);
    t

  let path_pt (Path p) i =
    if i >= 0 && i < Path64.length p
    then cpath_get_point p i
    else invalid_arg "path_pt: out of bounds access"

  let paths_pt (Paths ps) i j =
    if i >= 0 && i < Paths64.length ps && j >= 0 && j < Paths64.path_length ps i
    then Point.to_v @@ C.Funcs.paths64_get_point ps i j
    else invalid_arg "paths_pt: out of bounds access"

  let[@inline] ( .%() ) p i = path_pt p i
  let[@inline] ( .%{} ) ps (i, j) = paths_pt ps i j
  let[@inline] path vs = Path (cpath_of_contour vs)

  let paths ps =
    let c = Paths64.make () in
    List.iter (fun p -> Paths64.add_path c (cpath_of_contour p)) ps;
    Paths c

  let contour : type c l. (c, l) t -> l = function
    | Path p -> cpath_to_contour p
    | Paths ps ->
      List.init (Paths64.length ps) (fun i ->
        let len = Conv.size_to_int @@ C.Funcs.paths64_path_length ps i in
        Ctr.of_seq
        @@ Seq.init len (fun j -> Point.to_v @@ C.Funcs.paths64_get_point ps i j) )

  let ellipse ?(fn = 0) ?centre wh =
    let centre =
      match centre with
      | Some c -> Point.of_v c
      | None -> Point.make (Int64.of_int 0) (Int64.of_int 0)
    and buf, t = Path64.alloc ()
    and rx = (Int64.to_float @@ V.x wh) *. 0.5
    and ry = (Int64.to_float @@ V.y wh) *. 0.5 in
    let _ = C.Funcs.path64_ellipse buf centre rx ry fn in
    Path t

  let translate (type c l) v (t : (c, l) t) : (c, l) t =
    match t with
    | Path p ->
      let buf, translated = Path64.alloc () in
      let _ = C.Funcs.path64_translate buf p (V.x v) (V.y v) in
      Path translated
    | Paths ps ->
      let buf, translated = Paths64.alloc () in
      let _ = C.Funcs.paths64_translate buf ps (V.x v) (V.y v) in
      Paths translated

  let boolean_op
    (type c l)
    ?(fill_rule = fill_rule)
    ~op
    (subjects : (c, l) t)
    (clips : (c, l) t list)
    =
    let buf, t = Paths64.alloc ()
    and op = ClipType.make op
    and fill_rule = FillRule.make fill_rule
    and cs = Paths64.make () in
    let ss =
      match subjects with
      | Path s ->
        let ss = Paths64.make () in
        List.iter (fun (Path p) -> Paths64.add_path cs p) clips;
        Paths64.add_path ss s;
        ss
      | Paths ss ->
        List.iter
          (fun (Paths ps) ->
            let len = Paths64.length ps in
            for i = 0 to len - 1 do
              Paths64.add_path cs (Paths64.get_path ps i)
            done )
          clips;
        ss
    in
    let _ = C.Funcs.paths64_boolean_op buf op fill_rule ss cs in
    Paths t

  let boolean_op_tree
    (type c l)
    ?(fill_rule = fill_rule)
    ~op
    (subjects : (c, l) t)
    (clips : (c, l) t list)
    =
    let tree = PolyTree64.make ()
    and op = ClipType.make op
    and fill_rule = FillRule.make fill_rule
    and cs = Paths64.make () in
    let ss =
      match subjects with
      | Path s ->
        let ss = Paths64.make () in
        List.iter (fun (Path p) -> Paths64.add_path cs p) clips;
        Paths64.add_path ss s;
        ss
      | Paths ss ->
        List.iter
          (fun (Paths ps) ->
            let len = Paths64.length ps in
            for i = 0 to len - 1 do
              Paths64.add_path cs (Paths64.get_path ps i)
            done )
          clips;
        ss
    in
    let _ = C.Funcs.paths64_boolean_op_tree op fill_rule ss cs tree in
    tree

  let intersect ?fill_rule = function
    | [] -> Paths (Paths64.make ())
    | hd :: tl -> boolean_op ?fill_rule ~op:`Intersection hd tl

  let union ?fill_rule = function
    | [] -> Paths (Paths64.make ())
    | hd :: tl -> boolean_op ?fill_rule ~op:`Union hd tl

  let add ?fill_rule a b = boolean_op ?fill_rule ~op:`Union a [ b ]

  let difference ?fill_rule subjects clips =
    boolean_op ?fill_rule ~op:`Difference subjects clips

  let sub ?fill_rule a b = difference ?fill_rule a [ b ]

  let xor ?fill_rule = function
    | [] -> Paths (Paths64.make ())
    | hd :: tl -> boolean_op ?fill_rule ~op:`Xor hd tl

  let bounds (type c l) (t : (c, l) t) =
    let buf, rect = Rect64.alloc () in
    let _ =
      match t with
      | Path p -> C.Funcs.path64_bounds buf p
      | Paths ps -> C.Funcs.paths64_bounds buf ps
    in
    rect

  let rect_clip (type c l) ?(closed = true) rect (t : (c, l) t) =
    let buf, paths = Paths64.alloc () in
    let _ =
      match t, closed with
      | Path p, true -> C.Funcs.path64_rect_clip buf rect p
      | Path p, false -> C.Funcs.path64_rect_clip_line buf rect p
      | Paths ps, true -> C.Funcs.paths64_rect_clip buf rect ps
      | Paths ps, false -> C.Funcs.paths64_rect_clip_lines buf rect ps
    in
    Paths paths

  let inflate
    (type c l)
    ?(join_type = join_type)
    ?(end_type = end_type)
    ~delta
    (t : (c, l) t)
    =
    let buf, inflated = Paths64.alloc ()
    and join_type, miter_limit = JoinType.make join_type
    and end_type = EndType.make end_type in
    let ps =
      match t with
      | Path p ->
        let ps = Paths64.make () in
        Paths64.add_path ps p;
        ps
      | Paths ps -> ps
    in
    let _ = C.Funcs.paths64_inflate buf ps delta join_type end_type miter_limit in
    Paths inflated

  let trim_collinear (type c l) ?(closed = true) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p ->
      let buf, trimmed = Path64.alloc () in
      let _ = C.Funcs.path64_trim_collinear buf p closed in
      Path trimmed
    | Paths ps ->
      let trimmed = Paths64.make ()
      and len = Paths64.length ps in
      (* TODO: should make a function on the C side to cut down on copying? *)
      for i = 0 to len - 1 do
        let buf, td = Path64.alloc ()
        and p = Paths64.get_path ps i in
        let _ = C.Funcs.path64_trim_collinear buf p closed in
        Paths64.add_path trimmed td
      done;
      Paths trimmed

  let strip_near_equal (type c l) ?(closed = true) ?(eps = eps) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p ->
      let buf, stripped = Path64.alloc () in
      let _ = C.Funcs.path64_strip_near_equal buf p eps closed in
      Path stripped
    | Paths ps ->
      let buf, stripped = Paths64.alloc () in
      let _ = C.Funcs.paths64_strip_near_equal buf ps eps closed in
      Paths stripped

  let strip_duplicates (type c l) ?(closed = true) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p ->
      let buf, stripped = Path64.alloc () in
      let _ = C.Funcs.path64_strip_duplicates buf p closed in
      Path stripped
    | Paths ps ->
      let buf, stripped = Paths64.alloc () in
      let _ = C.Funcs.paths64_strip_duplicates buf ps closed in
      Paths stripped

  let simplify (type c l) ?(closed = true) ?(eps = eps) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p ->
      let buf, simplified = Path64.alloc () in
      let _ = C.Funcs.path64_simplify buf p eps closed in
      Path simplified
    | Paths ps ->
      let buf, simplified = Paths64.alloc () in
      let _ = C.Funcs.paths64_simplify buf ps eps closed in
      Paths simplified

  let ramer_douglas_peucker (type c l) ?(eps = eps) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p ->
      let buf, rdp = Path64.alloc () in
      let _ = C.Funcs.path64_ramer_douglas_peucker buf p eps in
      Path rdp
    | Paths ps ->
      let buf, rdp = Paths64.alloc () in
      let _ = C.Funcs.paths64_ramer_douglas_peucker buf ps eps in
      Paths rdp

  let area : type c l. (c, l) t -> float = function
    | Path p -> C.Funcs.path64_area p
    | Paths ps -> C.Funcs.paths64_area ps

  let point_inside (Path path) p =
    match C.Funcs.point64_in_path path (Point.of_v p) with
    | IsOutside -> `Outside
    | IsInside -> `Inside
    | IsOn -> `OnBorder

  let is_positive (Path p) = C.Funcs.path64_is_positive p

  let of_poly p =
    let c = Paths64.make () in
    List.iter (fun p -> Paths64.add_path c (cpath_of_list p)) (P.to_list p);
    Paths c

  let[@inline] of_polys ps =
    let c = Paths64.make () in
    List.fold_left (fun acc p -> List.rev_append (P.to_list p) acc) [] ps
    |> List.iter (fun p -> Paths64.add_path c (cpath_of_list p));
    Paths c

  let to_poly (Path p) = P.of_list [ cpath_to_list p ]

  let to_polys ?(fill_rule = fill_rule) t =
    let tree = boolean_op_tree ~fill_rule ~op:`Union t [] in
    let polys = PolyTree64.decompose cpath_to_list tree in
    List.map P.of_list polys

  let minkowski
    (type c l)
    ?(closed = true)
    ?(fill_rule = fill_rule)
    ~pattern:(Path pat)
    ~op
    (t : (c, l) t)
    : paths
    =
    let buf, mink = Paths64.alloc ()
    and fr = FillRule.make fill_rule in
    let _ =
      match t, op with
      | Path p, `Sum -> C.Funcs.path64_minkowski_sum buf pat p closed
      | Paths ps, `Sum -> C.Funcs.paths64_minkowski_sum buf pat ps closed fr
      | Path p, `Diff -> C.Funcs.path64_minkowski_diff buf pat p closed
      | Paths ps, `Diff -> C.Funcs.paths64_minkowski_diff buf pat ps closed fr
    in
    Paths mink

  let minkowski_sum ?closed ?fill_rule ~pattern t =
    minkowski ?closed ?fill_rule ~pattern ~op:`Sum t

  let minkowski_diff ?closed ?fill_rule ~pattern t =
    minkowski ?closed ?fill_rule ~pattern ~op:`Diff t
end

module Make64 (V : V with type n := int64) (Conf : Config) =
  Make64'
    (V)
    (struct
      type t = V.t list

      let of_seq s = List.of_seq s
      let to_seq l = List.to_seq l
    end)
    (struct
      type t = V.t list list

      let to_list = Fun.id
      let of_list = Fun.id
    end)
    (Conf)
