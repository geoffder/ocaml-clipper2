module Functions (F : Ctypes.FOREIGN) = struct
  open Ctypes
  open F
  open Types

  (** {1 Boolean Operations} *)

  let paths64_boolean_op =
    foreign
      "clipper_paths64_boolean_op"
      ( ptr void
      @-> ClipType.t
      @-> FillRule.t
      @-> ptr Paths64.t
      @-> ptr Paths64.t
      @-> returning (ptr Paths64.t) )

  let paths64_boolean_op_tree =
    foreign
      "clipper_paths64_boolean_op_tree"
      ( ClipType.t
      @-> FillRule.t
      @-> ptr Paths64.t
      @-> ptr Paths64.t
      @-> ptr PolyTree64.t
      @-> returning void )

  let paths64_intersect =
    foreign
      "clipper_paths64_intersect"
      ( ptr void
      @-> ptr Paths64.t
      @-> ptr Paths64.t
      @-> FillRule.t
      @-> returning (ptr Paths64.t) )

  let paths64_union =
    foreign
      "clipper_paths64_union"
      ( ptr void
      @-> ptr Paths64.t
      @-> ptr Paths64.t
      @-> FillRule.t
      @-> returning (ptr Paths64.t) )

  let paths64_difference =
    foreign
      "clipper_paths64_difference"
      ( ptr void
      @-> ptr Paths64.t
      @-> ptr Paths64.t
      @-> FillRule.t
      @-> returning (ptr Paths64.t) )

  let paths64_xor =
    foreign
      "clipper_paths64_xor"
      ( ptr void
      @-> ptr Paths64.t
      @-> ptr Paths64.t
      @-> FillRule.t
      @-> returning (ptr Paths64.t) )

  let pathsd_boolean_op =
    foreign
      "clipper_pathsd_boolean_op"
      ( ptr void
      @-> ClipType.t
      @-> FillRule.t
      @-> ptr PathsD.t
      @-> ptr PathsD.t
      @-> int
      @-> returning (ptr PathsD.t) )

  let pathsd_boolean_op_tree =
    foreign
      "clipper_pathsd_boolean_op_tree"
      ( ClipType.t
      @-> FillRule.t
      @-> ptr PathsD.t
      @-> ptr PathsD.t
      @-> ptr PolyTreeD.t
      @-> int
      @-> returning void )

  let pathsd_intersect =
    foreign
      "clipper_pathsd_intersect"
      ( ptr void
      @-> ptr PathsD.t
      @-> ptr PathsD.t
      @-> FillRule.t
      @-> int
      @-> returning (ptr PathsD.t) )

  let pathsd_union =
    foreign
      "clipper_pathsd_union"
      ( ptr void
      @-> ptr PathsD.t
      @-> ptr PathsD.t
      @-> FillRule.t
      @-> int
      @-> returning (ptr PathsD.t) )

  let pathsd_difference =
    foreign
      "clipper_pathsd_difference"
      ( ptr void
      @-> ptr PathsD.t
      @-> ptr PathsD.t
      @-> FillRule.t
      @-> int
      @-> returning (ptr PathsD.t) )

  let pathsd_xor =
    foreign
      "clipper_pathsd_xor"
      ( ptr void
      @-> ptr PathsD.t
      @-> ptr PathsD.t
      @-> FillRule.t
      @-> int
      @-> returning (ptr PathsD.t) )

  (** {1 Path Offsetting} *)

  let paths64_inflate =
    foreign
      "clipper_paths64_inflate"
      ( ptr void
      @-> ptr Paths64.t
      @-> double
      @-> JoinType.t
      @-> EndType.t
      @-> double
      @-> returning (ptr Paths64.t) )

  let pathsd_inflate =
    foreign
      "clipper_pathsd_inflate"
      ( ptr void
      @-> ptr PathsD.t
      @-> double
      @-> JoinType.t
      @-> EndType.t
      @-> double
      @-> int
      @-> returning (ptr PathsD.t) )

  (** {1 Rect Clipping} *)

  let path64_bounds =
    foreign
      "clipper_path64_bounds"
      (ptr void @-> ptr Path64.t @-> returning (ptr Rect64.t))

  let pathd_bounds =
    foreign "clipper_pathd_bounds" (ptr void @-> ptr PathD.t @-> returning (ptr RectD.t))

  let paths64_bounds =
    foreign
      "clipper_paths64_bounds"
      (ptr void @-> ptr Paths64.t @-> returning (ptr Rect64.t))

  let pathsd_bounds =
    foreign "clipper_pathsd_bounds" (ptr void @-> ptr PathsD.t @-> returning (ptr RectD.t))

  let path64_rect_clip =
    foreign
      "clipper_path64_rect_clip"
      (ptr void @-> ptr Rect64.t @-> ptr Path64.t @-> returning (ptr Paths64.t))

  let pathd_rect_clip =
    foreign
      "clipper_pathd_rect_clip"
      (ptr void @-> ptr RectD.t @-> ptr PathD.t @-> int @-> returning (ptr PathsD.t))

  let paths64_rect_clip =
    foreign
      "clipper_paths64_rect_clip"
      (ptr void @-> ptr Rect64.t @-> ptr Paths64.t @-> returning (ptr Paths64.t))

  let pathsd_rect_clip =
    foreign
      "clipper_pathsd_rect_clip"
      (ptr void @-> ptr RectD.t @-> ptr PathsD.t @-> int @-> returning (ptr PathsD.t))

  let path64_rect_clip_line =
    foreign
      "clipper_path64_rect_clip_line"
      (ptr void @-> ptr Rect64.t @-> ptr Path64.t @-> returning (ptr Paths64.t))

  let pathd_rect_clip_line =
    foreign
      "clipper_pathd_rect_clip_line"
      (ptr void @-> ptr RectD.t @-> ptr PathD.t @-> int @-> returning (ptr PathsD.t))

  let paths64_rect_clip_lines =
    foreign
      "clipper_paths64_rect_clip_lines"
      (ptr void @-> ptr Rect64.t @-> ptr Paths64.t @-> returning (ptr Paths64.t))

  let pathsd_rect_clip_lines =
    foreign
      "clipper_pathsd_rect_clip_lines"
      (ptr void @-> ptr RectD.t @-> ptr PathsD.t @-> int @-> returning (ptr PathsD.t))

  (** {1 Path Constructors} *)

  let path64 = foreign "clipper_path64" (ptr void @-> returning (ptr Path64.t))
  let pathd = foreign "clipper_pathd" (ptr void @-> returning (ptr PathD.t))

  let path64_of_points =
    foreign
      "clipper_path64_of_points"
      (ptr void @-> ptr Point64.t @-> size_t @-> returning (ptr Path64.t))

  let pathd_of_points =
    foreign
      "clipper_pathd_of_points"
      (ptr void @-> ptr PointD.t @-> size_t @-> returning (ptr PathD.t))

  let path64_add_point =
    foreign "clipper_path64_add_point" (ptr Path64.t @-> Point64.t @-> returning void)

  let pathd_add_point =
    foreign "clipper_pathd_add_point" (ptr PathD.t @-> PointD.t @-> returning void)

  let path64_ellipse =
    foreign
      "clipper_path64_ellipse"
      (ptr void @-> Point64.t @-> double @-> double @-> int @-> returning (ptr Path64.t))

  let pathd_ellipse =
    foreign
      "clipper_pathd_ellipse"
      (ptr void @-> PointD.t @-> double @-> double @-> int @-> returning (ptr PathD.t))

  let paths64 = foreign "clipper_paths64" (ptr void @-> returning (ptr Paths64.t))
  let pathsd = foreign "clipper_pathsd" (ptr void @-> returning (ptr PathsD.t))

  let paths64_of_paths =
    foreign
      "clipper_paths64_of_paths"
      (ptr void @-> ptr (ptr Path64.t) @-> size_t @-> returning (ptr Paths64.t))

  let pathsd_of_paths =
    foreign
      "clipper_pathsd_of_paths"
      (ptr void @-> ptr (ptr PathD.t) @-> size_t @-> returning (ptr PathsD.t))

  let paths64_add_path =
    foreign "clipper_paths64_add_path" (ptr Paths64.t @-> ptr Path64.t @-> returning void)

  let pathsd_add_path =
    foreign "clipper_pathsd_add_path" (ptr PathsD.t @-> ptr PathD.t @-> returning void)

  (** {1 Path Conversions (to C)} *)

  let path64_length = foreign "clipper_path64_length" (ptr Path64.t @-> returning size_t)
  let pathd_length = foreign "clipper_pathd_length" (ptr PathD.t @-> returning size_t)

  let path64_get_point =
    foreign "clipper_path64_get_point" (ptr Path64.t @-> int @-> returning Point64.t)

  let pathd_get_point =
    foreign "clipper_pathd_get_point" (ptr PathD.t @-> int @-> returning PointD.t)

  let path64_to_points =
    foreign
      "clipper_path64_to_points"
      (ptr void @-> ptr Path64.t @-> returning (ptr Point64.t))

  let pathd_to_points =
    foreign
      "clipper_pathd_to_points"
      (ptr void @-> ptr PathD.t @-> returning (ptr PointD.t))

  let paths64_length =
    foreign "clipper_paths64_length" (ptr Paths64.t @-> returning size_t)

  let pathsd_length = foreign "clipper_pathsd_length" (ptr PathsD.t @-> returning size_t)

  let paths64_lengths =
    foreign
      "clipper_paths64_lengths"
      (ptr void @-> ptr Paths64.t @-> returning (ptr size_t))

  let pathsd_lengths =
    foreign "clipper_pathsd_lengths" (ptr void @-> ptr PathsD.t @-> returning (ptr size_t))

  let paths64_path_length =
    foreign "clipper_paths64_path_length" (ptr Paths64.t @-> int @-> returning size_t)

  let pathsd_path_length =
    foreign "clipper_pathsd_path_length" (ptr PathsD.t @-> int @-> returning size_t)

  let paths64_get_path =
    foreign
      "clipper_paths64_get_path"
      (ptr void @-> ptr Paths64.t @-> int @-> returning (ptr Path64.t))

  let pathsd_get_path =
    foreign
      "clipper_pathsd_get_path"
      (ptr void @-> ptr PathsD.t @-> int @-> returning (ptr PathD.t))

  let paths64_get_point =
    foreign
      "clipper_paths64_get_point"
      (ptr Paths64.t @-> int @-> int @-> returning Point64.t)

  let pathsd_get_point =
    foreign
      "clipper_pathsd_get_point"
      (ptr PathsD.t @-> int @-> int @-> returning PointD.t)

  let paths64_to_points =
    foreign
      "clipper_paths64_to_points"
      (ptr (ptr void) @-> ptr Paths64.t @-> returning (ptr (ptr Point64.t)))

  let pathsd_to_points =
    foreign
      "clipper_pathsd_to_points"
      (ptr (ptr void) @-> ptr PathsD.t @-> returning (ptr (ptr PointD.t)))

  (** {1 Path Numeric Conversions} *)

  let path64_to_pathd =
    foreign
      "clipper_path64_to_pathd"
      (ptr void @-> ptr Path64.t @-> returning (ptr PathD.t))

  let pathd_to_path64 =
    foreign
      "clipper_pathd_to_path64"
      (ptr void @-> ptr PathD.t @-> returning (ptr Path64.t))

  let paths64_to_pathsd =
    foreign
      "clipper_paths64_to_pathsd"
      (ptr void @-> ptr Paths64.t @-> returning (ptr PathsD.t))

  let pathsd_to_paths64 =
    foreign
      "clipper_pathsd_to_paths64"
      (ptr void @-> ptr PathsD.t @-> returning (ptr Paths64.t))

  let scale_path64_to_pathd =
    foreign
      "clipper_scale_path64_to_pathd"
      ( ptr void
      @-> ptr Path64.t
      @-> float
      @-> float
      @-> ptr int
      @-> returning (ptr PathD.t) )

  let scale_pathd_to_path64 =
    foreign
      "clipper_scale_pathd_to_path64"
      ( ptr void
      @-> ptr PathD.t
      @-> float
      @-> float
      @-> ptr int
      @-> returning (ptr Path64.t) )

  let scale_paths64_to_pathsd =
    foreign
      "clipper_scale_paths64_to_pathsd"
      ( ptr void
      @-> ptr Paths64.t
      @-> float
      @-> float
      @-> ptr int
      @-> returning (ptr PathsD.t) )

  let scale_pathsd_to_paths64 =
    foreign
      "clipper_scale_pathsd_to_paths64"
      ( ptr void
      @-> ptr PathsD.t
      @-> float
      @-> float
      @-> ptr int
      @-> returning (ptr Paths64.t) )

  (** {1 Path Transformations} *)

  let path64_translate =
    foreign
      "clipper_path64_translate"
      (ptr void @-> ptr Path64.t @-> int64_t @-> int64_t @-> returning (ptr Path64.t))

  let pathd_translate =
    foreign
      "clipper_pathd_translate"
      (ptr void @-> ptr PathD.t @-> double @-> double @-> returning (ptr PathD.t))

  let paths64_translate =
    foreign
      "clipper_paths64_translate"
      (ptr void @-> ptr Paths64.t @-> int64_t @-> int64_t @-> returning (ptr Paths64.t))

  let pathsd_translate =
    foreign
      "clipper_pathsd_translate"
      (ptr void @-> ptr PathsD.t @-> double @-> double @-> returning (ptr PathsD.t))

  let path64_trim_collinear =
    foreign
      "clipper_path64_trim_collinear"
      (ptr void @-> ptr Path64.t @-> bool @-> returning (ptr Path64.t))

  let pathd_trim_collinear =
    foreign
      "clipper_pathd_trim_collinear"
      (ptr void @-> ptr PathD.t @-> bool @-> int @-> returning (ptr PathD.t))

  let pathd_strip_near_equal =
    foreign
      "clipper_pathd_strip_near_equal"
      (ptr void @-> ptr PathD.t @-> double @-> bool @-> returning (ptr PathD.t))

  let path64_strip_near_equal =
    foreign
      "clipper_path64_strip_near_equal"
      (ptr void @-> ptr Path64.t @-> double @-> bool @-> returning (ptr Path64.t))

  let pathsd_strip_near_equal =
    foreign
      "clipper_pathsd_strip_near_equal"
      (ptr void @-> ptr PathsD.t @-> double @-> bool @-> returning (ptr PathsD.t))

  let paths64_strip_near_equal =
    foreign
      "clipper_paths64_strip_near_equal"
      (ptr void @-> ptr Paths64.t @-> double @-> bool @-> returning (ptr Paths64.t))

  let pathd_strip_duplicates =
    foreign
      "clipper_pathd_strip_duplicates"
      (ptr void @-> ptr PathD.t @-> bool @-> returning (ptr PathD.t))

  let path64_strip_duplicates =
    foreign
      "clipper_path64_strip_duplicates"
      (ptr void @-> ptr Path64.t @-> bool @-> returning (ptr Path64.t))

  let pathsd_strip_duplicates =
    foreign
      "clipper_pathsd_strip_duplicates"
      (ptr void @-> ptr PathsD.t @-> bool @-> returning (ptr PathsD.t))

  let paths64_strip_duplicates =
    foreign
      "clipper_paths64_strip_duplicates"
      (ptr void @-> ptr Paths64.t @-> bool @-> returning (ptr Paths64.t))

  let path64_simplify =
    foreign
      "clipper_path64_simplify"
      (ptr void @-> ptr Path64.t @-> double @-> bool @-> returning (ptr Path64.t))

  let pathd_simplify =
    foreign
      "clipper_pathd_simplify"
      (ptr void @-> ptr PathD.t @-> double @-> bool @-> returning (ptr PathD.t))

  let paths64_simplify =
    foreign
      "clipper_paths64_simplify"
      (ptr void @-> ptr Paths64.t @-> double @-> bool @-> returning (ptr Paths64.t))

  let pathsd_simplify =
    foreign
      "clipper_pathsd_simplify"
      (ptr void @-> ptr PathsD.t @-> double @-> bool @-> returning (ptr PathsD.t))

  let path64_ramer_douglas_peucker =
    foreign
      "clipper_path64_ramer_douglas_peucker"
      (ptr void @-> ptr Path64.t @-> double @-> returning (ptr Path64.t))

  let pathd_ramer_douglas_peucker =
    foreign
      "clipper_pathd_ramer_douglas_peucker"
      (ptr void @-> ptr PathD.t @-> double @-> returning (ptr PathD.t))

  let paths64_ramer_douglas_peucker =
    foreign
      "clipper_paths64_ramer_douglas_peucker"
      (ptr void @-> ptr Paths64.t @-> double @-> returning (ptr Paths64.t))

  let pathsd_ramer_douglas_peucker =
    foreign
      "clipper_pathsd_ramer_douglas_peucker"
      (ptr void @-> ptr PathsD.t @-> double @-> returning (ptr PathsD.t))

  (** {1 Minkowski} *)

  let path64_minkowski_sum =
    foreign
      "clipper_path64_minkowski_sum"
      (ptr void @-> ptr Path64.t @-> ptr Path64.t @-> bool @-> returning (ptr Paths64.t))

  let pathd_minkowski_sum =
    foreign
      "clipper_pathd_minkowski_sum"
      ( ptr void
      @-> ptr PathD.t
      @-> ptr PathD.t
      @-> bool
      @-> int
      @-> returning (ptr PathsD.t) )

  let path64_minkowski_diff =
    foreign
      "clipper_path64_minkowski_diff"
      (ptr void @-> ptr Path64.t @-> ptr Path64.t @-> bool @-> returning (ptr Paths64.t))

  let pathd_minkowski_diff =
    foreign
      "clipper_pathd_minkowski_diff"
      ( ptr void
      @-> ptr PathD.t
      @-> ptr PathD.t
      @-> bool
      @-> int
      @-> returning (ptr PathsD.t) )

  let paths64_minkowski_sum =
    foreign
      "clipper_paths64_minkowski_sum"
      ( ptr void
      @-> ptr Path64.t
      @-> ptr Paths64.t
      @-> bool
      @-> FillRule.t
      @-> returning (ptr Paths64.t) )

  let pathsd_minkowski_sum =
    foreign
      "clipper_pathsd_minkowski_sum"
      ( ptr void
      @-> ptr PathD.t
      @-> ptr PathsD.t
      @-> bool
      @-> int
      @-> FillRule.t
      @-> returning (ptr PathsD.t) )

  let paths64_minkowski_diff =
    foreign
      "clipper_paths64_minkowski_diff"
      ( ptr void
      @-> ptr Path64.t
      @-> ptr Paths64.t
      @-> bool
      @-> FillRule.t
      @-> returning (ptr Paths64.t) )

  let pathsd_minkowski_diff =
    foreign
      "clipper_pathsd_minkowski_diff"
      ( ptr void
      @-> ptr PathD.t
      @-> ptr PathsD.t
      @-> bool
      @-> int
      @-> FillRule.t
      @-> returning (ptr PathsD.t) )

  (** {1 Geometry} *)

  let point64_distance =
    foreign "clipper_point64_distance" (Point64.t @-> Point64.t @-> returning double)

  let pointd_distance =
    foreign "clipper_pointd_distance" (PointD.t @-> PointD.t @-> returning double)

  let point64_near_collinear =
    foreign
      "clipper_point64_near_collinear"
      (Point64.t @-> Point64.t @-> Point64.t @-> double @-> returning bool)

  let pointd_near_collinear =
    foreign
      "clipper_pointd_near_collinear"
      (PointD.t @-> PointD.t @-> PointD.t @-> double @-> returning bool)

  let point64_in_path =
    foreign
      "clipper_point_in_path64"
      (ptr Path64.t @-> Point64.t @-> returning PointInPolygon.t)

  let pointd_in_path =
    foreign
      "clipper_point_in_pathd"
      (ptr PathD.t @-> PointD.t @-> returning PointInPolygon.t)

  let path64_area = foreign "clipper_path64_area" (ptr Path64.t @-> returning double)
  let pathd_area = foreign "clipper_pathd_area" (ptr PathD.t @-> returning double)
  let paths64_area = foreign "clipper_paths64_area" (ptr Paths64.t @-> returning double)
  let pathsd_area = foreign "clipper_pathsd_area" (ptr PathsD.t @-> returning double)

  let pathd_is_positive =
    foreign "clipper_pathd_is_positive" (ptr PathD.t @-> returning bool)

  let path64_is_positive =
    foreign "clipper_path64_is_positive" (ptr Path64.t @-> returning bool)

  (** {1 PolyTree Class} *)

  (** {2 constructors} *)

  let polytree64 =
    foreign
      "clipper_polytree64"
      (ptr void @-> ptr PolyTree64.t @-> returning (ptr PolyTree64.t))

  let polytreed =
    foreign
      "clipper_polytreed"
      (ptr void @-> ptr PolyTreeD.t @-> returning (ptr PolyTreeD.t))

  (** {2 int64 methods} *)

  let polytree64_parent =
    foreign "clipper_polytree64_parent" (ptr PolyTree64.t @-> returning (ptr PolyTree64.t))

  let polytree64_get_child =
    foreign
      "clipper_polytree64_get_child"
      (ptr PolyTree64.t @-> size_t @-> returning (ptr PolyTree64.t))

  let polytree64_add_child =
    foreign
      "clipper_polytree64_add_child"
      (ptr PolyTree64.t @-> ptr Path64.t @-> returning (ptr PolyTree64.t))

  let polytree64_clear =
    foreign "clipper_polytree64_clear" (ptr PolyTree64.t @-> returning void)

  let polytree64_count =
    foreign "clipper_polytree64_count" (ptr PolyTree64.t @-> returning size_t)

  let polytree64_level =
    foreign "clipper_polytree64_level" (ptr PolyTree64.t @-> returning int)

  let polytree64_is_hole =
    foreign "clipper_polytree64_is_hole" (ptr PolyTree64.t @-> returning bool)

  let polytree64_polygon =
    foreign
      "clipper_polytree64_polygon"
      (ptr void @-> ptr PolyTree64.t @-> returning (ptr Path64.t))

  let polytree64_area =
    foreign "clipper_polytree64_area" (ptr PolyTree64.t @-> returning double)

  let polytree64_to_paths =
    foreign
      "clipper_polytree64_to_paths"
      (ptr void @-> ptr PolyTree64.t @-> returning (ptr Paths64.t))

  let polytree64_fully_contains_children =
    foreign
      "clipper_polytree64_fully_contains_children"
      (ptr PolyTree64.t @-> returning bool)

  (** {2 decimal methods} *)

  let polytreed_parent =
    foreign "clipper_polytreed_parent" (ptr PolyTreeD.t @-> returning (ptr PolyTreeD.t))

  let polytreed_get_child =
    foreign
      "clipper_polytreed_get_child"
      (ptr PolyTreeD.t @-> size_t @-> returning (ptr PolyTreeD.t))

  let polytreed_add_child =
    foreign
      "clipper_polytreed_add_child"
      (ptr PolyTreeD.t @-> ptr Path64.t @-> returning (ptr PolyTreeD.t))

  let polytreed_clear =
    foreign "clipper_polytreed_clear" (ptr PolyTreeD.t @-> returning void)

  let polytreed_count =
    foreign "clipper_polytreed_count" (ptr PolyTreeD.t @-> returning size_t)

  let polytreed_level =
    foreign "clipper_polytreed_level" (ptr PolyTreeD.t @-> returning int)

  let polytreed_is_hole =
    foreign "clipper_polytreed_is_hole" (ptr PolyTreeD.t @-> returning bool)

  let polytreed_polygon =
    foreign
      "clipper_polytreed_polygon"
      (ptr void @-> ptr PolyTreeD.t @-> returning (ptr PathD.t))

  let polytreed_area =
    foreign "clipper_polytreed_area" (ptr PolyTreeD.t @-> returning double)

  let polytreed_to_paths =
    foreign
      "clipper_polytreed_to_paths"
      (ptr void @-> ptr PolyTreeD.t @-> returning (ptr PathsD.t))

  let polytreed_set_inv_scale =
    foreign
      "clipper_polytreed_set_inv_scale"
      (ptr PolyTreeD.t @-> double @-> returning void)

  let polytreed_inv_scale =
    foreign "clipper_polytreed_inv_scale" (ptr PolyTreeD.t @-> returning double)

  (** {1 Rect Class} *)

  (** {2 constructors} *)

  let rect64 =
    foreign
      "clipper_rect64"
      ( ptr void
      @-> int64_t
      @-> int64_t
      @-> int64_t
      @-> int64_t
      @-> returning (ptr Rect64.t) )

  let rectd =
    foreign
      "clipper_rectd"
      (ptr void @-> double @-> double @-> double @-> double @-> returning (ptr RectD.t))

  (** {2 int64 methods} *)

  let rect64_width = foreign "clipper_rect64_width" (ptr Rect64.t @-> returning int64_t)
  let rect64_height = foreign "clipper_rect64_height" (ptr Rect64.t @-> returning int64_t)

  let rect64_midpoint =
    foreign "clipper_rect64_midpoint" (ptr Rect64.t @-> returning Point64.t)

  let rect64_as_path =
    foreign
      "clipper_rect64_as_path"
      (ptr void @-> ptr Rect64.t @-> returning (ptr Path64.t))

  let rect64_contains_pt =
    foreign "clipper_rect64_contains_pt" (ptr Rect64.t @-> Point64.t @-> returning bool)

  let rect64_contains_rect =
    foreign
      "clipper_rect64_contains_rect"
      (ptr Rect64.t @-> ptr Rect64.t @-> returning bool)

  let rect64_scale =
    foreign
      "clipper_rect64_scale"
      (ptr void @-> ptr Rect64.t @-> double @-> returning (ptr Rect64.t))

  let rect64_is_empty = foreign "clipper_rect64_is_empty" (ptr Rect64.t @-> returning bool)

  let rect64_intersects =
    foreign "clipper_rect64_intersects" (ptr Rect64.t @-> ptr Rect64.t @-> returning bool)

  (** {2 decimal methods} *)

  let rectd_width = foreign "clipper_rectd_width" (ptr RectD.t @-> returning double)
  let rectd_height = foreign "clipper_rectd_height" (ptr RectD.t @-> returning double)

  let rectd_midpoint =
    foreign "clipper_rectd_midpoint" (ptr RectD.t @-> returning PointD.t)

  let rectd_as_path =
    foreign "clipper_rectd_as_path" (ptr void @-> ptr RectD.t @-> returning (ptr PathD.t))

  let rectd_contains_pt =
    foreign "clipper_rectd_contains_pt" (ptr RectD.t @-> PointD.t @-> returning bool)

  let rectd_contains_rect =
    foreign "clipper_rectd_contains_rect" (ptr RectD.t @-> ptr RectD.t @-> returning bool)

  let rectd_scale =
    foreign
      "clipper_rectd_scale"
      (ptr void @-> ptr RectD.t @-> double @-> returning (ptr RectD.t))

  let rectd_is_empty = foreign "clipper_rectd_is_empty" (ptr RectD.t @-> returning bool)

  let rectd_intersects =
    foreign "clipper_rectd_intersects" (ptr RectD.t @-> ptr RectD.t @-> returning bool)

  (** {1 Clipper Class} *)

  (** {2 contsructors} *)

  let clipper64 = foreign "clipper_clipper64" (ptr void @-> returning (ptr Clipper64.t))

  let clipperd =
    foreign "clipper_clipperd" (ptr void @-> int @-> returning (ptr ClipperD.t))

  (** {2 int64 accessors} *)

  let clipper64_set_preserve_collinear =
    foreign
      "clipper_clipper64_set_preserve_collinear"
      (ptr Clipper64.t @-> bool @-> returning void)

  let clipper64_set_reverse_solution =
    foreign
      "clipper_clipper64_set_reverse_solution"
      (ptr Clipper64.t @-> bool @-> returning void)

  let clipper64_get_preserve_collinear =
    foreign "clipper_clipper64_get_preserve_collinear" (ptr Clipper64.t @-> returning bool)

  let clipper64_get_reverse_solution =
    foreign "clipper_clipper64_get_reverse_solution" (ptr Clipper64.t @-> returning bool)

  let clipper64_clear =
    foreign "clipper_clipper64_clear" (ptr Clipper64.t @-> returning void)

  (** {2 decimal accessors} *)

  let clipperd_set_preserve_collinear =
    foreign
      "clipper_clipperd_set_preserve_collinear"
      (ptr ClipperD.t @-> bool @-> returning void)

  let clipperd_set_reverse_solution =
    foreign
      "clipper_clipperd_set_reverse_solution"
      (ptr ClipperD.t @-> bool @-> returning void)

  let clipperd_get_preserve_collinear =
    foreign "clipper_clipperd_get_preserve_collinear" (ptr ClipperD.t @-> returning bool)

  let clipperd_get_reverse_solution =
    foreign "clipper_clipperd_get_reverse_solution" (ptr ClipperD.t @-> returning bool)

  let clipperd_clear = foreign "clipper_clipperd_clear" (ptr ClipperD.t @-> returning void)

  (** {2 int64 methods} *)

  let clipper64_add_subject =
    foreign
      "clipper_clipper64_add_subject"
      (ptr Clipper64.t @-> ptr Paths64.t @-> returning void)

  let clipper64_add_open_subject =
    foreign
      "clipper_clipper64_add_open_subject"
      (ptr Clipper64.t @-> ptr Paths64.t @-> returning void)

  let clipper64_add_clip =
    foreign
      "clipper_clipper64_add_clip"
      (ptr Clipper64.t @-> ptr Paths64.t @-> returning void)

  let clipper64_execute =
    foreign
      "clipper_clipper64_execute"
      ( ptr Clipper64.t
      @-> ClipType.t
      @-> FillRule.t
      @-> ptr Paths64.t
      @-> ptr Paths64.t
      @-> returning bool )

  let clipper64_execute_tree =
    foreign
      "clipper_clipper64_execute_tree"
      ( ptr Clipper64.t
      @-> ClipType.t
      @-> FillRule.t
      @-> ptr PolyTree64.t
      @-> returning bool )

  let clipper64_execute_tree_with_open =
    foreign
      "clipper_clipper64_execute_tree_with_open"
      ( ptr Clipper64.t
      @-> ClipType.t
      @-> FillRule.t
      @-> ptr PolyTree64.t
      @-> ptr Paths64.t
      @-> returning bool )

  (** {2 decimal methods} *)

  let clipperd_add_subject =
    foreign
      "clipper_clipperd_add_subject"
      (ptr ClipperD.t @-> ptr PathsD.t @-> returning void)

  let clipperd_add_open_subject =
    foreign
      "clipper_clipperd_add_open_subject"
      (ptr ClipperD.t @-> ptr PathsD.t @-> returning void)

  let clipperd_add_clip =
    foreign
      "clipper_clipperd_add_clip"
      (ptr ClipperD.t @-> ptr PathsD.t @-> returning void)

  let clipperd_execute =
    foreign
      "clipper_clipperd_execute"
      ( ptr ClipperD.t
      @-> ClipType.t
      @-> FillRule.t
      @-> ptr PathsD.t
      @-> ptr PathsD.t
      @-> returning bool )

  let clipperd_execute_tree =
    foreign
      "clipper_clipperd_execute_tree"
      (ptr ClipperD.t @-> ClipType.t @-> FillRule.t @-> ptr PolyTreeD.t @-> returning bool)

  let clipperd_execute_tree_with_open =
    foreign
      "clipper_clipperd_execute_tree_with_open"
      ( ptr ClipperD.t
      @-> ClipType.t
      @-> FillRule.t
      @-> ptr PolyTreeD.t
      @-> ptr PathsD.t
      @-> returning bool )

  (** {1 Offset Class} *)

  (** {2 constructors} *)

  let offset =
    foreign
      "clipper_clipperoffset"
      (ptr void @-> double @-> double @-> bool @-> bool @-> returning (ptr Offset.t))

  (** {2 accessors} *)

  let offset_set_miter_limit =
    foreign
      "clipper_clipperoffset_set_miter_limit"
      (ptr Offset.t @-> double @-> returning void)

  let offset_set_arc_tolerance =
    foreign
      "clipper_clipperoffset_set_arc_tolerance"
      (ptr Offset.t @-> double @-> returning void)

  let offset_set_preserve_collinear =
    foreign
      "clipper_clipperoffset_set_preserve_collinear"
      (ptr Offset.t @-> bool @-> returning void)

  let offset_set_reverse_solution =
    foreign
      "clipper_clipperoffset_set_reverse_solution"
      (ptr Offset.t @-> bool @-> returning void)

  let offset_get_miter_limit =
    foreign "clipper_clipperoffset_get_miter_limit" (ptr Offset.t @-> returning double)

  let offset_get_arc_tolerance =
    foreign "clipper_clipperoffset_get_arc_tolerance" (ptr Offset.t @-> returning double)

  let offset_get_preserve_collinear =
    foreign
      "clipper_clipperoffset_get_preserve_collinear"
      (ptr Offset.t @-> returning bool)

  let offset_get_reverse_solution =
    foreign "clipper_clipperoffset_get_reverse_solution" (ptr Offset.t @-> returning bool)

  let offset_error_code =
    foreign "clipper_clipperoffset_error_code" (ptr Offset.t @-> returning int)

  let offset_clear =
    foreign "clipper_clipperoffset_clear" (ptr Offset.t @-> returning void)

  (** {2 methods} *)

  let offset_add_pathd =
    foreign
      "clipper_clipperoffset_add_pathd"
      (ptr Offset.t @-> ptr PathD.t @-> JoinType.t @-> EndType.t @-> returning void)

  let offset_add_pathsd =
    foreign
      "clipper_clipperoffset_add_pathsd"
      (ptr Offset.t @-> ptr PathsD.t @-> JoinType.t @-> EndType.t @-> returning void)

  let offset_add_path64 =
    foreign
      "clipper_clipperoffset_add_path64"
      (ptr Offset.t @-> ptr Path64.t @-> JoinType.t @-> EndType.t @-> returning void)

  let offset_add_paths64 =
    foreign
      "clipper_clipperoffset_add_paths64"
      (ptr Offset.t @-> ptr Paths64.t @-> JoinType.t @-> EndType.t @-> returning void)

  let offset_execute =
    foreign
      "clipper_clipperoffset_execute"
      (ptr void @-> ptr Offset.t @-> double @-> returning (ptr Paths64.t))

  (** {1 SvgWriter Class} *)

  (** {2 contsructors} *)

  let svgwriter =
    foreign "clipper_svgwriter" (ptr void @-> int @-> returning (ptr SvgWriter.t))

  (** {2 accessors / setters} *)

  let svgwriter_fill_rule =
    foreign "clipper_svgwriter_fill_rule" (ptr SvgWriter.t @-> returning FillRule.t)

  let svgwriter_set_coords_style =
    foreign
      "clipper_svgwriter_set_coords_style"
      (ptr SvgWriter.t @-> ptr char @-> uint32_t @-> uint32_t @-> returning void)

  (** {2 methods} *)

  let svgwriter_add_text =
    foreign
      "clipper_svgwriter_add_text"
      ( ptr SvgWriter.t
      @-> ptr char
      @-> uint32_t
      @-> uint32_t
      @-> int
      @-> int
      @-> returning void )

  let svgwriter_add_path64 =
    foreign
      "clipper_svgwriter_add_path64"
      ( ptr SvgWriter.t
      @-> ptr Path64.t
      @-> bool
      @-> FillRule.t
      @-> uint32_t
      @-> uint32_t
      @-> double
      @-> bool
      @-> returning void )

  let svgwriter_add_pathd =
    foreign
      "clipper_svgwriter_add_pathd"
      ( ptr SvgWriter.t
      @-> ptr PathD.t
      @-> bool
      @-> FillRule.t
      @-> uint32_t
      @-> uint32_t
      @-> double
      @-> bool
      @-> returning void )

  let svgwriter_add_paths64 =
    foreign
      "clipper_svgwriter_add_paths64"
      ( ptr SvgWriter.t
      @-> ptr Paths64.t
      @-> bool
      @-> FillRule.t
      @-> uint32_t
      @-> uint32_t
      @-> double
      @-> bool
      @-> returning void )

  let svgwriter_add_pathsd =
    foreign
      "clipper_svgwriter_add_pathsd"
      ( ptr SvgWriter.t
      @-> ptr PathsD.t
      @-> bool
      @-> FillRule.t
      @-> uint32_t
      @-> uint32_t
      @-> double
      @-> bool
      @-> returning void )

  let svgwriter_save_to_file =
    foreign
      "clipper_svgwriter_save_to_file"
      (ptr SvgWriter.t @-> ptr char @-> int @-> int @-> int @-> returning bool)

  let svgwriter_clear =
    foreign "clipper_svgwriter_clear" (ptr SvgWriter.t @-> returning void)

  (** {1 SvgReader Class} *)

  (** {2 contsructors} *)

  let svgreader = foreign "clipper_svgreader" (ptr void @-> returning (ptr SvgReader.t))

  (** {2 accessors} *)

  let svgreader_get_pathsd =
    foreign
      "clipper_svgreader_get_pathsd"
      (ptr void @-> ptr SvgReader.t @-> returning (ptr PathsD.t))

  (** {2 methods} *)

  let svgreader_load_from_file =
    foreign
      "clipper_svgreader_load_from_file"
      (ptr SvgReader.t @-> ptr char @-> returning void)

  let svgreader_clear =
    foreign "clipper_svgreader_clear" (ptr SvgReader.t @-> returning void)

  (** {1 memory size} *)

  let path64_size = foreign "clipper_path64_size" (void @-> returning size_t)
  let pathd_size = foreign "clipper_pathd_size" (void @-> returning size_t)
  let paths64_size = foreign "clipper_paths64_size" (void @-> returning size_t)
  let pathsd_size = foreign "clipper_pathsd_size" (void @-> returning size_t)
  let rect64_size = foreign "clipper_rect64_size" (void @-> returning size_t)
  let rectd_size = foreign "clipper_rectd_size" (void @-> returning size_t)
  let polytree64_size = foreign "clipper_polytree64_size" (void @-> returning size_t)
  let polytreed_size = foreign "clipper_polytreed_size" (void @-> returning size_t)
  let clipper64_size = foreign "clipper_clipper64_size" (void @-> returning size_t)
  let clipperd_size = foreign "clipper_clipperd_size" (void @-> returning size_t)
  let offset_size = foreign "clipper_clipperoffset_size" (void @-> returning size_t)
  let svgwriter_size = foreign "clipper_svgwriter_size" (void @-> returning size_t)
  let svgreader_size = foreign "clipper_svgreader_size" (void @-> returning size_t)

  (** {1 destruction} *)

  let destruct_path64 = foreign "clipper_destruct_path64" (ptr Path64.t @-> returning void)
  let destruct_pathd = foreign "clipper_destruct_pathd" (ptr PathD.t @-> returning void)

  let destruct_paths64 =
    foreign "clipper_destruct_paths64" (ptr Paths64.t @-> returning void)

  let destruct_pathsd = foreign "clipper_destruct_pathsd" (ptr PathsD.t @-> returning void)
  let destruct_rect64 = foreign "clipper_destruct_rect64" (ptr Rect64.t @-> returning void)
  let destruct_rectd = foreign "clipper_destruct_rectd" (ptr RectD.t @-> returning void)

  let destruct_polytree64 =
    foreign "clipper_destruct_polytree64" (ptr PolyTree64.t @-> returning void)

  let destruct_polytreed =
    foreign "clipper_destruct_polytreed" (ptr PolyTreeD.t @-> returning void)

  let destruct_clipper64 =
    foreign "clipper_destruct_clipper64" (ptr Clipper64.t @-> returning void)

  let destruct_clipperd =
    foreign "clipper_destruct_clipperd" (ptr ClipperD.t @-> returning void)

  let destruct_offset =
    foreign "clipper_destruct_clipperoffset" (ptr Offset.t @-> returning void)

  let destruct_svgwriter =
    foreign "clipper_destruct_svgwriter" (ptr SvgWriter.t @-> returning void)

  let destruct_svgreader =
    foreign "clipper_destruct_svgreader" (ptr SvgReader.t @-> returning void)

  (** {1 pointer free + destruction} *)

  let delete_path64 = foreign "clipper_delete_path64" (ptr Path64.t @-> returning void)
  let delete_pathd = foreign "clipper_delete_pathd" (ptr PathD.t @-> returning void)
  let delete_paths64 = foreign "clipper_delete_paths64" (ptr Paths64.t @-> returning void)
  let delete_pathsd = foreign "clipper_delete_pathsd" (ptr PathsD.t @-> returning void)
  let delete_rect64 = foreign "clipper_delete_rect64" (ptr Rect64.t @-> returning void)
  let delete_rectd = foreign "clipper_delete_rectd" (ptr RectD.t @-> returning void)

  let delete_polytree64 =
    foreign "clipper_delete_polytree64" (ptr PolyTree64.t @-> returning void)

  let delete_polytreed =
    foreign "clipper_delete_polytreed" (ptr PolyTreeD.t @-> returning void)

  let delete_clipper64 =
    foreign "clipper_delete_clipper64" (ptr Clipper64.t @-> returning void)

  let delete_clipperd =
    foreign "clipper_delete_clipperd" (ptr ClipperD.t @-> returning void)

  let delete_clipperoffset =
    foreign "clipper_delete_clipperoffset" (ptr Offset.t @-> returning void)

  let delete_svgwriter =
    foreign "clipper_delete_svgwriter" (ptr SvgWriter.t @-> returning void)

  let delete_svgreader =
    foreign "clipper_delete_svgreader" (ptr SvgReader.t @-> returning void)
end
