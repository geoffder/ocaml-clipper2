include Clipper_intf
include ConfigTypes
open Clpr

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
  type ('cpp, 'ctr) t =
    | Path : PathD.t -> ([ `Path ], Ctr.t) t
    | Paths : PathsD.t -> ([ `Paths ], Ctr.t list) t

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

    let of_pts a b = of_pts (Point.of_v a) (Point.of_v b)
    let midpoint t = Point.to_v @@ midpoint t
    let as_path t = Path (as_path t)
    let contains_pt t p = contains_pt t (Point.of_v p)
  end

  let n_pts : type c l. (c, l) t -> int = function
    | Path p -> PathD.length p
    | Paths ps ->
      let n = ref 0 in
      let len = PathsD.length ps in
      for i = 0 to len - 1 do
        n := !n + PathD.length (PathsD.unsafe_subpath ps i)
      done;
      !n

  let n_pts_sub (Paths ps) i = PathsD.sublength ps i

  let n_paths : type c l. (c, l) t -> int = function
    | Path _ -> 1
    | Paths ps -> PathsD.length ps

  let[@inline] cpath_get_point t i = Point.to_v @@ PathD.unsafe_get t i
  let cpath_to_seq c = Seq.init (PathD.length c) (cpath_get_point c)

  let cpath_of_seq vs =
    let t = PathD.make () in
    Seq.iter (fun v -> PathD.add_point t (Point.of_v v)) vs;
    t

  let cpath_to_contour t = Ctr.of_seq @@ Seq.init (PathD.length t) (cpath_get_point t)

  let cpath_of_contour vs =
    let t = PathD.make () in
    Seq.iter (fun v -> PathD.add_point t (Point.of_v v)) (Ctr.to_seq vs);
    t

  let subpath (Paths ps) i = Path (PathsD.subpath ps i)

  let path_pt (Path p) i =
    if i >= 0 && i < PathD.length p
    then cpath_get_point p i
    else invalid_arg "path_pt: out of bounds access"

  let paths_pt (Paths ps) i j =
    if i >= 0 && i < PathsD.length ps && j >= 0 && j < PathsD.unsafe_sublength ps i
    then Point.to_v @@ PathsD.unsafe_get ps i j
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
        let len = PathsD.unsafe_sublength ps i in
        Ctr.of_seq @@ Seq.init len (fun j -> Point.to_v @@ PathsD.unsafe_get ps i j) )

  let ellipse ?fn ?centre wh =
    Path (PathD.ellipse ?fn ?centre:(Option.map Point.of_v centre) (V.x wh) (V.y wh))

  let translate (type c l) v (t : (c, l) t) : (c, l) t =
    match t with
    | Path p -> Path (PathD.translate p (V.x v) (V.y v))
    | Paths ps -> Paths (PathsD.translate ps (V.x v) (V.y v))

  let map (type c l) (f : V.t -> V.t) (t : (c, l) t) : (c, l) t =
    let map f p =
      let m = PathD.make ()
      and len = PathD.length p in
      for i = 0 to len - 1 do
        let v = Point.to_v @@ PathD.unsafe_get p i in
        PathD.add_point m (Point.of_v @@ f v)
      done;
      m
    in
    match t with
    | Path p -> Path (map f p)
    | Paths ps ->
      let ms = PathsD.make ()
      and len = PathsD.length ps in
      for i = 0 to len - 1 do
        PathsD.add_path ms (map f @@ PathsD.unsafe_subpath ps i)
      done;
      Paths ms

  let boolean_op
    (type c l)
    ?(fill_rule = fill_rule)
    ~op
    (subjects : (c, l) t)
    (clips : (c, l) t list)
    =
    let cs = PathsD.make () in
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
              PathsD.add_path cs (PathsD.unsafe_subpath ps i)
            done )
          clips;
        ss
    in
    Paths (PathsD.boolean_op ~precision ~fill_rule ~op ss cs)

  let boolean_op_tree
    (type c l)
    ?(fill_rule = fill_rule)
    ~op
    (subjects : (c, l) t)
    (clips : (c, l) t list)
    =
    let cs = PathsD.make () in
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
              PathsD.add_path cs (PathsD.unsafe_subpath ps i)
            done )
          clips;
        ss
    in
    PathsD.boolean_op_tree ~precision ~fill_rule ~op ss cs

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

  let bounds : type c l. (c, l) t -> RectD.t = function
    | Path p -> PathD.bounds p
    | Paths ps -> PathsD.bounds ps

  let rect_clip (type c l) ?closed rect (t : (c, l) t) =
    match t with
    | Path p -> Paths (PathD.rect_clip ~precision ?closed p rect)
    | Paths ps -> Paths (PathsD.rect_clip ~precision ?closed ps rect)

  let inflate
    (type c l)
    ?miter_limit
    ?(join_type = join_type)
    ?(end_type = end_type)
    ~delta
    (t : (c, l) t)
    =
    let ps =
      match t with
      | Path p -> PathsD.of_path p
      | Paths ps -> ps
    in
    Paths (PathsD.inflate ~precision ?miter_limit ~join_type ~end_type ~delta ps)

  let trim_collinear (type c l) ?closed (t : (c, l) t) : (c, l) t =
    match t with
    | Path p -> Path (PathD.trim_collinear ~precision ?closed p)
    | Paths ps ->
      let trimmed = PathsD.make ()
      and len = PathsD.length ps in
      (* TODO: should make a function on the C side to cut down on copying? *)
      for i = 0 to len - 1 do
        let p = PathsD.unsafe_subpath ps i in
        PathsD.add_path trimmed (PathD.trim_collinear ~precision ?closed p)
      done;
      Paths trimmed

  let strip_near_equal (type c l) ?closed ?(eps = eps) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p -> Path (PathD.strip_near_equal ?closed ~eps p)
    | Paths ps -> Paths (PathsD.strip_near_equal ?closed ~eps ps)

  let strip_duplicates (type c l) ?closed (t : (c, l) t) : (c, l) t =
    match t with
    | Path p -> Path (PathD.strip_duplicates ?closed p)
    | Paths ps -> Paths (PathsD.strip_duplicates ?closed ps)

  let simplify (type c l) ?closed ?(eps = eps) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p -> Path (PathD.simplify ?closed ~eps p)
    | Paths ps -> Paths (PathsD.simplify ?closed ~eps ps)

  let ramer_douglas_peucker (type c l) ?(eps = eps) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p -> Path (PathD.ramer_douglas_peucker ~eps p)
    | Paths ps -> Paths (PathsD.ramer_douglas_peucker ~eps ps)

  let area : type c l. (c, l) t -> float = function
    | Path p -> PathD.area p
    | Paths ps -> PathsD.area ps

  let point_inside (Path path) p = PathD.point_inside path (Point.of_v p)
  let is_positive (Path p) = PathD.is_positive p

  let of_poly p =
    let c = PathsD.make () in
    Seq.iter (fun p -> PathsD.add_path c (cpath_of_seq p)) (P.to_seq p);
    Paths c

  let[@inline] of_polys ps =
    let c = PathsD.make () in
    List.fold_left (fun acc p -> Seq.append (P.to_seq p) acc) Seq.empty ps
    |> Seq.iter (fun p -> PathsD.add_path c (cpath_of_seq p));
    Paths c

  let to_poly (Path p) = P.of_seq (Seq.return @@ cpath_to_seq p)

  let to_polys ?(fill_rule = fill_rule) t =
    let tree = boolean_op_tree ~fill_rule ~op:`Union t [] in
    let polys = PolyTreeD.decompose cpath_to_seq tree in
    List.of_seq @@ Seq.map P.of_seq polys

  let minkowski
    (type c l)
    ?closed
    ?(fill_rule = fill_rule)
    ~pattern:(Path pattern)
    ~op
    (t : (c, l) t)
    : paths
    =
    match t, op with
    | Path p, `Sum -> Paths (PathD.minkowski_sum ~precision ?closed ~pattern p)
    | Paths ps, `Sum ->
      Paths (PathsD.minkowski_sum ~precision ~fill_rule ?closed ~pattern ps)
    | Path p, `Diff -> Paths (PathD.minkowski_diff ~precision ?closed ~pattern p)
    | Paths ps, `Diff ->
      Paths (PathsD.minkowski_diff ~precision ~fill_rule ?closed ~pattern ps)

  let minkowski_sum ?closed ?fill_rule ~pattern t =
    minkowski ?closed ?fill_rule ~pattern ~op:`Sum t

  let minkowski_diff ?closed ?fill_rule ~pattern t =
    minkowski ?closed ?fill_rule ~pattern ~op:`Diff t

  module Svg = struct
    module Color = Color

    type coords =
      { font : string option
      ; color : Color.t option
      ; size : int option
      }

    type artist = A : (SvgWriter.t -> unit) -> artist

    let color ?alpha c = Color.make ?alpha c
    let coords ?font ?size ?color () = { font; color; size }

    let text ?(size = 11) ?(color = color ~alpha:1. Black) pos text =
      A
        (fun w ->
          SvgWriter.add_text
            w
            ~font_size:size
            ~font_color:(Color.to_int color)
            ~x:(Int.of_float @@ V.x pos)
            ~y:(Int.of_float @@ V.y pos)
            text )

    let paint
      (type c l)
      ?closed
      ?(fill_rule = fill_rule)
      ?show_coords
      ?width
      ?(brush = color ~alpha:0.1 Blue)
      ?(pen = color ~alpha:0.8 (RGB (179, 179, 218)))
      (t : (c, l) t)
      =
      let brush_color = Color.to_int brush
      and pen_color = Color.to_int pen in
      match t with
      | Path p ->
        A
          (fun w ->
            SvgWriter.add_pathd
              ?closed
              ~fill_rule
              ?show_coords
              ?pen_width:width
              ~brush_color
              ~pen_color
              w
              p )
      | Paths ps ->
        A
          (fun w ->
            SvgWriter.add_pathsd
              ?closed
              ~fill_rule
              ?show_coords
              ?pen_width:width
              ~brush_color
              ~pen_color
              w
              ps )

    let subject ?(closed = true) ?fill_rule t =
      let brush = color ~alpha:0.1 (Hex 0x00009C)
      and pen = color ~alpha:0.8 (Hex 0xB3B3DA)
      and width = if closed then 0.8 else 1.3 in
      paint ~closed ?fill_rule ~brush ~pen ~width t

    let clip ?fill_rule t =
      let brush = color ~alpha:0.07 (Hex 0x9C0000)
      and pen = color ~alpha:0.8 (Hex 0xFFA07A) in
      paint ~closed:true ?fill_rule ~brush ~pen ~width:0.8 t

    let solution ?(closed = true) ?fill_rule ?show_coords t =
      let brush = color ~alpha:0.25 (Hex 0x66FF66)
      and pen = color ~alpha:1. (Hex (if closed then 0x003300 else 0x006600))
      and width = if closed then 1.2 else 1.8 in
      paint ~closed ?fill_rule ?show_coords ~brush ~pen ~width t

    let write ?max_width ?max_height ?margin ?coords filename artists =
      let w = SvgWriter.make ~precision () in
      Option.iter
        (fun { font = font_name; size = font_size; color } ->
          let font_color = Option.map Color.to_int color in
          SvgWriter.set_coords_style ?font_name ?font_size ?font_color w )
        coords;
      List.iter (fun (A f) -> f w) artists;
      SvgWriter.save ?max_width ?max_height ?margin w filename

    let read path =
      let rdr = SvgReader.make () in
      SvgReader.load rdr path;
      Paths (SvgReader.get_pathsd rdr)
  end
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

      let of_seq s = List.of_seq @@ Seq.map List.of_seq s
      let to_seq t = Seq.map List.to_seq @@ List.to_seq t
    end)
    (Conf)

module Make64'
  (V : V with type n := int64)
  (Ctr : Contour with type v := V.t)
  (P : Poly with type v := V.t)
  (Conf : Config) =
struct
  type ('cpp, 'ctr) t =
    | Path : Path64.t -> ([ `Path ], Ctr.t) t
    | Paths : Paths64.t -> ([ `Paths ], Ctr.t list) t

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

    let of_pts a b = of_pts (Point.of_v a) (Point.of_v b)
    let midpoint t = Point.to_v @@ midpoint t
    let as_path t = Path (as_path t)
    let contains_pt t p = contains_pt t (Point.of_v p)
  end

  let n_pts : type c l. (c, l) t -> int = function
    | Path p -> Path64.length p
    | Paths ps ->
      let n = ref 0 in
      let len = Paths64.length ps in
      for i = 0 to len - 1 do
        n := !n + Path64.length (Paths64.unsafe_subpath ps i)
      done;
      !n

  let n_pts_sub (Paths ps) i = Paths64.sublength ps i

  let n_paths : type c l. (c, l) t -> int = function
    | Path _ -> 1
    | Paths ps -> Paths64.length ps

  let[@inline] cpath_get_point t i = Point.to_v @@ Path64.unsafe_get t i
  let cpath_to_seq c = Seq.init (Path64.length c) (cpath_get_point c)

  let cpath_of_seq vs =
    let t = Path64.make () in
    Seq.iter (fun v -> Path64.add_point t (Point.of_v v)) vs;
    t

  let cpath_to_contour t = Ctr.of_seq @@ Seq.init (Path64.length t) (cpath_get_point t)

  let cpath_of_contour vs =
    let t = Path64.make () in
    Seq.iter (fun v -> Path64.add_point t (Point.of_v v)) (Ctr.to_seq vs);
    t

  let subpath (Paths ps) i = Path (Paths64.subpath ps i)

  let path_pt (Path p) i =
    if i >= 0 && i < Path64.length p
    then cpath_get_point p i
    else invalid_arg "path_pt: out of bounds access"

  let paths_pt (Paths ps) i j =
    if i >= 0 && i < Paths64.length ps && j >= 0 && j < Paths64.unsafe_sublength ps i
    then Point.to_v @@ Paths64.unsafe_get ps i j
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
        let len = Paths64.unsafe_sublength ps i in
        Ctr.of_seq @@ Seq.init len (fun j -> Point.to_v @@ Paths64.unsafe_get ps i j) )

  let ellipse ?fn ?centre wh =
    Path (Path64.ellipse ?fn ?centre:(Option.map Point.of_v centre) (V.x wh) (V.y wh))

  let translate (type c l) v (t : (c, l) t) : (c, l) t =
    match t with
    | Path p -> Path (Path64.translate p (V.x v) (V.y v))
    | Paths ps -> Paths (Paths64.translate ps (V.x v) (V.y v))

  let map (type c l) (f : V.t -> V.t) (t : (c, l) t) : (c, l) t =
    let map f p =
      let m = Path64.make ()
      and len = Path64.length p in
      for i = 0 to len - 1 do
        let v = Point.to_v @@ Path64.unsafe_get p i in
        Path64.add_point m (Point.of_v @@ f v)
      done;
      m
    in
    match t with
    | Path p -> Path (map f p)
    | Paths ps ->
      let ms = Paths64.make ()
      and len = Paths64.length ps in
      for i = 0 to len - 1 do
        Paths64.add_path ms (map f @@ Paths64.unsafe_subpath ps i)
      done;
      Paths ms

  let boolean_op
    (type c l)
    ?(fill_rule = fill_rule)
    ~op
    (subjects : (c, l) t)
    (clips : (c, l) t list)
    =
    let cs = Paths64.make () in
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
              Paths64.add_path cs (Paths64.unsafe_subpath ps i)
            done )
          clips;
        ss
    in
    Paths (Paths64.boolean_op ~fill_rule ~op ss cs)

  let boolean_op_tree
    (type c l)
    ?(fill_rule = fill_rule)
    ~op
    (subjects : (c, l) t)
    (clips : (c, l) t list)
    =
    let cs = Paths64.make () in
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
              Paths64.add_path cs (Paths64.unsafe_subpath ps i)
            done )
          clips;
        ss
    in
    Paths64.boolean_op_tree ~fill_rule ~op ss cs

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

  let bounds : type c l. (c, l) t -> Rect64.t = function
    | Path p -> Path64.bounds p
    | Paths ps -> Paths64.bounds ps

  let rect_clip (type c l) ?closed rect (t : (c, l) t) =
    match t with
    | Path p -> Paths (Path64.rect_clip ?closed p rect)
    | Paths ps -> Paths (Paths64.rect_clip ?closed ps rect)

  let inflate
    (type c l)
    ?miter_limit
    ?(join_type = join_type)
    ?(end_type = end_type)
    ~delta
    (t : (c, l) t)
    =
    let ps =
      match t with
      | Path p -> Paths64.of_path p
      | Paths ps -> ps
    in
    Paths (Paths64.inflate ?miter_limit ~join_type ~end_type ~delta ps)

  let trim_collinear (type c l) ?closed (t : (c, l) t) : (c, l) t =
    match t with
    | Path p -> Path (Path64.trim_collinear ?closed p)
    | Paths ps ->
      let trimmed = Paths64.make ()
      and len = Paths64.length ps in
      (* TODO: should make a function on the C side to cut down on copying? *)
      for i = 0 to len - 1 do
        let p = Paths64.unsafe_subpath ps i in
        Paths64.add_path trimmed (Path64.trim_collinear ?closed p)
      done;
      Paths trimmed

  let strip_near_equal (type c l) ?closed ?(eps = eps) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p -> Path (Path64.strip_near_equal ?closed ~eps p)
    | Paths ps -> Paths (Paths64.strip_near_equal ?closed ~eps ps)

  let strip_duplicates (type c l) ?closed (t : (c, l) t) : (c, l) t =
    match t with
    | Path p -> Path (Path64.strip_duplicates ?closed p)
    | Paths ps -> Paths (Paths64.strip_duplicates ?closed ps)

  let simplify (type c l) ?closed ?(eps = eps) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p -> Path (Path64.simplify ?closed ~eps p)
    | Paths ps -> Paths (Paths64.simplify ?closed ~eps ps)

  let ramer_douglas_peucker (type c l) ?(eps = eps) (t : (c, l) t) : (c, l) t =
    match t with
    | Path p -> Path (Path64.ramer_douglas_peucker ~eps p)
    | Paths ps -> Paths (Paths64.ramer_douglas_peucker ~eps ps)

  let area : type c l. (c, l) t -> float = function
    | Path p -> Path64.area p
    | Paths ps -> Paths64.area ps

  let point_inside (Path path) p = Path64.point_inside path (Point.of_v p)
  let is_positive (Path p) = Path64.is_positive p

  let of_poly p =
    let c = Paths64.make () in
    Seq.iter (fun p -> Paths64.add_path c (cpath_of_seq p)) (P.to_seq p);
    Paths c

  let[@inline] of_polys ps =
    let c = Paths64.make () in
    List.fold_left (fun acc p -> Seq.append (P.to_seq p) acc) Seq.empty ps
    |> Seq.iter (fun p -> Paths64.add_path c (cpath_of_seq p));
    Paths c

  let to_poly (Path p) = P.of_seq (Seq.return @@ cpath_to_seq p)

  let to_polys ?(fill_rule = fill_rule) t =
    let tree = boolean_op_tree ~fill_rule ~op:`Union t [] in
    let polys = PolyTree64.decompose cpath_to_seq tree in
    List.of_seq @@ Seq.map P.of_seq polys

  let minkowski
    (type c l)
    ?closed
    ?(fill_rule = fill_rule)
    ~pattern:(Path pattern)
    ~op
    (t : (c, l) t)
    : paths
    =
    match t, op with
    | Path p, `Sum -> Paths (Path64.minkowski_sum ?closed ~pattern p)
    | Paths ps, `Sum -> Paths (Paths64.minkowski_sum ~fill_rule ?closed ~pattern ps)
    | Path p, `Diff -> Paths (Path64.minkowski_diff ?closed ~pattern p)
    | Paths ps, `Diff -> Paths (Paths64.minkowski_diff ~fill_rule ?closed ~pattern ps)

  let minkowski_sum ?closed ?fill_rule ~pattern t =
    minkowski ?closed ?fill_rule ~pattern ~op:`Sum t

  let minkowski_diff ?closed ?fill_rule ~pattern t =
    minkowski ?closed ?fill_rule ~pattern ~op:`Diff t

  module Svg = struct
    module Color = Color

    type coords =
      { font : string option
      ; color : Color.t option
      ; size : int option
      }

    type artist = A : (SvgWriter.t -> unit) -> artist

    let color ?alpha c = Color.make ?alpha c
    let coords ?font ?size ?color () = { font; color; size }

    let text ?(size = 11) ?(color = color ~alpha:1. Black) pos text =
      A
        (fun w ->
          SvgWriter.add_text
            w
            ~font_size:size
            ~font_color:(Color.to_int color)
            ~x:(Int64.to_int @@ V.x pos)
            ~y:(Int64.to_int @@ V.y pos)
            text )

    let paint
      (type c l)
      ?closed
      ?(fill_rule = fill_rule)
      ?show_coords
      ?width
      ?(brush = color ~alpha:0.1 Blue)
      ?(pen = color ~alpha:0.8 (RGB (179, 179, 218)))
      (t : (c, l) t)
      =
      let brush_color = Color.to_int brush
      and pen_color = Color.to_int pen in
      match t with
      | Path p ->
        A
          (fun w ->
            SvgWriter.add_path64
              ?closed
              ~fill_rule
              ?show_coords
              ?pen_width:width
              ~brush_color
              ~pen_color
              w
              p )
      | Paths ps ->
        A
          (fun w ->
            SvgWriter.add_paths64
              ?closed
              ~fill_rule
              ?show_coords
              ?pen_width:width
              ~brush_color
              ~pen_color
              w
              ps )

    let subject ?(closed = true) ?fill_rule t =
      let brush = color ~alpha:0.1 (Hex 0x00009C)
      and pen = color ~alpha:0.8 (Hex 0xB3B3DA)
      and width = if closed then 0.8 else 1.3 in
      paint ~closed ?fill_rule ~brush ~pen ~width t

    let clip ?fill_rule t =
      let brush = color ~alpha:0.07 (Hex 0x9C0000)
      and pen = color ~alpha:0.8 (Hex 0xFFA07A) in
      paint ~closed:true ?fill_rule ~brush ~pen ~width:0.8 t

    let solution ?(closed = true) ?fill_rule ?show_coords t =
      let brush = color ~alpha:0.25 (Hex 0x66FF66)
      and pen = color ~alpha:1. (Hex (if closed then 0x003300 else 0x006600))
      and width = if closed then 1.2 else 1.8 in
      paint ~closed ?fill_rule ?show_coords ~brush ~pen ~width t

    let write ?max_width ?max_height ?margin ?coords filename artists =
      let w = SvgWriter.make () in
      Option.iter
        (fun { font = font_name; size = font_size; color } ->
          let font_color = Option.map Color.to_int color in
          SvgWriter.set_coords_style ?font_name ?font_size ?font_color w )
        coords;
      List.iter (fun (A f) -> f w) artists;
      SvgWriter.save ?max_width ?max_height ?margin w filename

    let read path =
      let rdr = SvgReader.make () in
      SvgReader.load rdr path;
      Paths (Result.get_ok @@ PathsD.to_paths64 @@ SvgReader.get_pathsd rdr)
  end
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

      let of_seq s = List.of_seq @@ Seq.map List.of_seq s
      let to_seq t = Seq.map List.to_seq @@ List.to_seq t
    end)
    (Conf)
