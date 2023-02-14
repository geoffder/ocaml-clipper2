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

module MakeD' (V : VD) (P : Poly with type v := V.t) (Conf : Config) = struct
  type path = C.Types.PathD.t Ctypes_static.ptr
  type paths = C.Types.PathsD.t Ctypes_static.ptr

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

    let make a b =
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
    let scale t s = C.Funcs.rectd_scale t s

    let as_path t =
      let buf, p = PathD.alloc () in
      let _ = C.Funcs.rectd_as_path buf t in
      p

    let contains_pt t p = C.Funcs.rectd_contains_pt t (Point.of_v p)
    let contains_rect a b = C.Funcs.rectd_contains_rect a b
    let is_empty t = C.Funcs.rectd_is_empty t
    let intersects a b = C.Funcs.rectd_intersects a b
  end

  module Path = struct
    include PathD

    let[@inline] get_point t i = Point.to_v @@ C.Funcs.pathd_get_point t i
    let[@inline] add_point t v = C.Funcs.pathd_add_point t (Point.of_v v)

    let of_list vs =
      let t = make () in
      List.iter (add_point t) vs;
      t

    let to_list t = List.init (length t) (get_point t)

    let ellipse ?(fn = 0) ?centre rad =
      let centre =
        match centre with
        | Some c -> Point.of_v c
        | None -> Point.make 0. 0.
      and buf, t = alloc () in
      let _ = C.Funcs.pathd_ellipse buf centre (V.x rad) (V.y rad) fn in
      t

    let translate v t =
      let buf, translated = alloc () in
      let _ = C.Funcs.pathd_translate buf t (V.x v) (V.y v) in
      translated

    let bounds t =
      let buf, rect = RectD.alloc () in
      let _ = C.Funcs.pathd_bounds buf t in
      rect

    let rect_clip rect t =
      let buf, clipped = PathD.alloc () in
      let _ = C.Funcs.pathd_rect_clip buf rect t precision in
      clipped

    let rect_clip_line rect t =
      let buf, clipped = PathsD.alloc () in
      let _ = C.Funcs.pathsd_rect_clip_line buf rect t precision in
      clipped

    let simplify ?(closed = true) ?(eps = eps) t =
      let buf, simplified = alloc () in
      let _ = C.Funcs.pathd_simplify buf t eps (not closed) in
      simplified

    let ramer_douglas_peucker ?(eps = eps) t =
      let buf, rdp = alloc () in
      let _ = C.Funcs.pathd_ramer_douglas_peucker buf t eps in
      rdp

    let strip_near_equal ?(closed = true) ?(eps = eps) t =
      let buf, stripped = alloc () in
      let _ = C.Funcs.pathd_strip_near_equal buf t eps closed in
      stripped

    let strip_duplicates ?(closed = true) t =
      let buf, stripped = alloc () in
      let _ = C.Funcs.pathd_strip_duplicates buf t closed in
      stripped

    let trim_collinear ?(closed = true) t =
      let buf, trimmed = alloc () in
      let _ = C.Funcs.pathd_trim_collinear buf t (not closed) precision in
      trimmed

    let point_inside t p =
      match C.Funcs.pointd_in_path t (Point.of_v p) with
      | IsOutside -> `Outside
      | IsInside -> `Inside
      | IsOn -> `OnBorder

    let area t = C.Funcs.pathd_area t
    let is_positive t = C.Funcs.pathd_is_positive t
  end

  module Paths = struct
    include PathsD

    let[@inline] get_point t i j = Point.to_v @@ C.Funcs.pathsd_get_point t i j

    let of_list paths =
      let t = make () in
      List.iter (fun p -> add_path t (Path.of_list p)) paths;
      t

    let to_list t =
      List.init (length t) (fun i ->
          let len = Conv.size_to_int @@ C.Funcs.pathsd_path_length t i in
          List.init len (fun j -> get_point t i j) )

    let translate v t =
      let buf, translated = alloc () in
      let _ = C.Funcs.pathsd_translate buf t (V.x v) (V.y v) in
      translated

    let boolean_op ?(fill_rule = fill_rule) ~op subjects clips =
      let buf, t = alloc ()
      and op = ClipType.make op
      and fill_rule = FillRule.make fill_rule in
      let _ = C.Funcs.pathsd_boolean_op buf op fill_rule subjects clips precision in
      t

    let boolean_op_tree ?(fill_rule = fill_rule) ~op subjects clips =
      let tree = PolyTreeD.make ()
      and op = ClipType.make op
      and fill_rule = FillRule.make fill_rule in
      let _ = C.Funcs.pathsd_boolean_op_tree op fill_rule subjects clips tree precision in
      tree

    let intersect ?(fill_rule = fill_rule) subjects clips =
      let buf, t = alloc ()
      and fill_rule = FillRule.make fill_rule in
      let _ = C.Funcs.pathsd_intersect buf subjects clips fill_rule precision in
      t

    let add ?(fill_rule = fill_rule) subjects clips =
      let buf, t = alloc ()
      and fill_rule = FillRule.make fill_rule in
      let _ = C.Funcs.pathsd_union buf subjects clips fill_rule precision in
      t

    let[@inline] union ?fill_rule subjects = add ?fill_rule subjects (make ())

    let difference ?(fill_rule = fill_rule) subjects clips =
      let buf, t = alloc ()
      and fill_rule = FillRule.make fill_rule in
      let _ = C.Funcs.pathsd_difference buf subjects clips fill_rule precision in
      t

    let[@inline] sub ?fill_rule subjects clips = difference ?fill_rule subjects clips

    let xor ?(fill_rule = fill_rule) subjects clips =
      let buf, t = alloc ()
      and fill_rule = FillRule.make fill_rule in
      let _ = C.Funcs.pathsd_xor buf subjects clips fill_rule precision in
      t

    let bounds t =
      let buf, rect = RectD.alloc () in
      let _ = C.Funcs.pathsd_bounds buf t in
      rect

    let rect_clip ?(closed = true) rect t =
      let buf, paths = PathsD.alloc () in
      let _ =
        if closed
        then C.Funcs.pathsd_rect_clip buf rect t precision
        else C.Funcs.pathsd_rect_clip_lines buf rect t precision
      in
      paths

    let inflate ?(join_type = join_type) ?(end_type = end_type) ~delta t =
      let buf, inflated = alloc ()
      and join_type, miter_limit = JoinType.make join_type
      and end_type = EndType.make end_type in
      let _ =
        C.Funcs.pathsd_inflate buf t delta join_type end_type miter_limit precision
      in
      inflated

    let strip_near_equal ?(closed = true) ?(eps = eps) t =
      let buf, stripped = alloc () in
      let _ = C.Funcs.pathsd_strip_near_equal buf t eps closed in
      stripped

    let strip_duplicates ?(closed = true) t =
      let buf, stripped = alloc () in
      let _ = C.Funcs.pathsd_strip_duplicates buf t closed in
      stripped

    let simplify ?(closed = true) ?(eps = eps) t =
      let buf, simplified = alloc () in
      let _ = C.Funcs.pathsd_simplify buf t eps (not closed) in
      simplified

    let ramer_douglas_peucker ?(eps = eps) t =
      let buf, rdp = alloc () in
      let _ = C.Funcs.pathsd_ramer_douglas_peucker buf t eps in
      rdp

    let area t = C.Funcs.pathsd_area t
    let[@inline] of_poly p = of_list @@ P.to_list p

    let[@inline] of_polys ps =
      of_list @@ List.fold_left (fun acc p -> List.rev_append (P.to_list p) acc) [] ps

    let to_polys ?(fill_rule = fill_rule) t =
      let tree = boolean_op_tree ~fill_rule ~op:`Union t (make ()) in
      let polys = PolyTreeD.decompose Path.to_list tree in
      List.map P.of_list polys
  end
end

module MakeD (V : VD) (Conf : Config) =
  MakeD'
    (V)
    (struct
      type t = V.t list list

      let to_list = Fun.id
      let of_list = Fun.id
    end)
    (Conf)

module Make64' (V : V64) (P : Poly with type v := V.t) (Conf : Config) = struct
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

    let make a b =
      let buf, t = alloc ()
      and left = Int64.min (V.x a) (V.x b)
      and right = Int64.max (V.x a) (V.x b)
      and bottom = Int64.min (V.y a) (V.y b)
      and top = Int64.max (V.y a) (V.y b) in
      let _ = C.Funcs.rect64 buf left top right bottom in
      t

    let width t = Int64.to_float @@ C.Funcs.rect64_width t
    let height t = Int64.to_float @@ C.Funcs.rect64_height t
    let midpoint t = Point.to_v @@ C.Funcs.rect64_midpoint t
    let scale t s = C.Funcs.rect64_scale t s

    let as_path t =
      let buf, p = Path64.alloc () in
      let _ = C.Funcs.rect64_as_path buf t in
      p

    let contains_pt t p = C.Funcs.rect64_contains_pt t (Point.of_v p)
    let contains_rect a b = C.Funcs.rect64_contains_rect a b
    let is_empty t = C.Funcs.rect64_is_empty t
    let intersects a b = C.Funcs.rect64_intersects a b
  end

  module Path = struct
    include Path64

    let get_point t i = Point.to_v @@ C.Funcs.path64_get_point t i
    let add_point t v = C.Funcs.path64_add_point t (Point.of_v v)

    let of_list ps =
      let t = make () in
      List.iter (add_point t) ps;
      t

    let to_list t =
      List.init (length t) (fun i -> Point.to_v @@ C.Funcs.path64_get_point t i)

    let ellipse ?(fn = 0) ?centre rad =
      let centre =
        match centre with
        | Some c -> Point.of_v c
        | None -> Point.make (Int64.of_int 0) (Int64.of_int 0)
      and buf, t = alloc ()
      and rx = Int64.to_float @@ V.x rad
      and ry = Int64.to_float @@ V.y rad in
      let _ = C.Funcs.path64_ellipse buf centre rx ry fn in
      t

    let translate v t =
      let buf, translated = alloc () in
      let _ = C.Funcs.path64_translate buf t (V.x v) (V.y v) in
      translated

    let bounds t =
      let buf, rect = Rect64.alloc () in
      let _ = C.Funcs.path64_bounds buf t in
      rect

    let rect_clip rect t =
      let buf, clipped = Path64.alloc () in
      let _ = C.Funcs.path64_rect_clip buf rect t in
      clipped

    let rect_clip_line rect t =
      let buf, clipped = Paths64.alloc () in
      let _ = C.Funcs.paths64_rect_clip_line buf rect t in
      clipped

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

  module Paths = struct
    include Paths64

    let translate v t =
      let buf, translated = alloc () in
      let _ = C.Funcs.paths64_translate buf t (V.x v) (V.y v) in
      translated

    let[@inline] get_point t i j = Point.to_v @@ C.Funcs.paths64_get_point t i j

    let of_list paths =
      let t = make () in
      List.iter (fun p -> add_path t (Path.of_list p)) paths;
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
      let polys = PolyTree64.decompose Path.to_list tree in
      List.map P.of_list polys
  end
end

module Make64 (V : V64) (Conf : Config) =
  Make64'
    (V)
    (struct
      type t = V.t list list

      let to_list = Fun.id
      let of_list = Fun.id
    end)
    (Conf)