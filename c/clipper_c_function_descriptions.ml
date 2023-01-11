module Functions (F : Ctypes.FOREIGN) = struct
  open Ctypes
  open F
  open Clipper_c_types

  (* Boolean Operations *)

  let paths64_boolean_op =
    foreign
      "clipper_paths64_boolean_op"
      ( ptr void
      @-> ClipType.t
      @-> FillRule.t
      @-> ptr Paths64.t
      @-> ptr Paths64.t
      @-> returning (ptr Paths64.t) )

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

  (* Path Offsetting *)

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

  (* Rect Clipping *)

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
      (ptr void @-> ptr Rect64.t @-> ptr Path64.t @-> returning (ptr Path64.t))

  let pathd_rect_clip =
    foreign
      "clipper_pathd_rect_clip"
      (ptr void @-> ptr RectD.t @-> ptr PathD.t @-> int @-> returning (ptr PathD.t))

  let paths64_rect_clip_line =
    foreign
      "clipper_paths64_rect_clip_line"
      (ptr void @-> ptr Rect64.t @-> ptr Path64.t @-> returning (ptr Paths64.t))

  let pathsd_rect_clip_line =
    foreign
      "clipper_pathsd_rect_clip_line"
      (ptr void @-> ptr RectD.t @-> ptr PathD.t @-> int @-> returning (ptr PathsD.t))

  let paths64_rect_clip_lines =
    foreign
      "clipper_paths64_rect_clip_lines"
      (ptr void @-> ptr Rect64.t @-> ptr Paths64.t @-> returning (ptr Paths64.t))

  let pathsd_rect_clip_lines =
    foreign
      "clipper_pathsd_rect_clip_lines"
      (ptr void @-> ptr RectD.t @-> ptr PathsD.t @-> int @-> returning (ptr PathsD.t))

  (* Path Constructors *)

  let path64 = foreign "clipper_path64" (ptr void @-> returning (ptr Path64.t))
  let pathd = foreign "clipper_pathd" (ptr void @-> returning (ptr PathD.t))

  let path64_of_string =
    foreign "clipper_path64_of_string" (ptr void @-> ptr char @-> returning (ptr Path64.t))

  let pathd_of_string =
    foreign "clipper_pathd_of_string" (ptr void @-> ptr char @-> returning (ptr PathD.t))

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
      (ptr void @-> Point64.t @-> double @-> double @-> int @-> returning void)

  let pathd_ellipse =
    foreign
      "clipper_pathd_ellipse"
      (ptr void @-> PointD.t @-> double @-> double @-> int @-> returning void)

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

  (* Path Conversions (to C) *)

  let path64_length = foreign "clipper_path64_length" (ptr Path64.t @-> returning size_t)
  let pathd_length = foreign "clipper_pathd_length" (ptr PathD.t @-> returning size_t)

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

  let paths64_get =
    foreign
      "clipper_paths64_get"
      (ptr void @-> ptr Paths64.t @-> int @-> returning (ptr Path64.t))

  let pathsd_get =
    foreign
      "clipper_pathsd_get"
      (ptr void @-> ptr PathsD.t @-> int @-> returning (ptr PathD.t))

  let paths64_to_points =
    foreign
      "clipper_paths64_to_points"
      (ptr (ptr void) @-> ptr Paths64.t @-> returning (ptr (ptr Point64.t)))

  let pathsd_to_points =
    foreign
      "clipper_pathsd_to_points"
      (ptr (ptr void) @-> ptr PathsD.t @-> returning (ptr (ptr PointD.t)))

  (* Path Transformations *)

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
      (ptr void @-> ptr Path64.t @-> int @-> returning (ptr Path64.t))

  let pathd_trim_collinear =
    foreign
      "clipper_pathd_trim_collinear"
      (ptr void @-> ptr PathD.t @-> int @-> int @-> returning (ptr PathD.t))

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

  (* Minkowski *)

  let path64_minkowski_sum =
    foreign
      "clipper_path64_minkowski_sum"
      (ptr void @-> ptr Path64.t @-> ptr Path64.t @-> int @-> returning (ptr Paths64.t))

  let pathd_minkowski_sum =
    foreign
      "clipper_pathd_minkowski_sum"
      ( ptr void
      @-> ptr PathD.t
      @-> ptr PathD.t
      @-> int
      @-> int
      @-> returning (ptr PathsD.t) )

  let path64_minkowski_diff =
    foreign
      "clipper_path64_minkowski_diff"
      (ptr void @-> ptr Path64.t @-> ptr Path64.t @-> int @-> returning (ptr Paths64.t))

  let pathd_minkowski_diff =
    foreign
      "clipper_pathd_minkowski_diff"
      ( ptr void
      @-> ptr PathD.t
      @-> ptr PathD.t
      @-> int
      @-> int
      @-> returning (ptr PathsD.t) )

  (* Geometry *)

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

  (* Class Interfaces *)

  (* PolyTree Constructors *)

  let polytree64 =
    foreign
      "clipper_polytree64"
      (ptr void @-> ptr PolyTree64.t @-> returning (ptr PolyTree64.t))

  let polytreed =
    foreign
      "clipper_polytreed"
      (ptr void @-> ptr PolyTreeD.t @-> returning (ptr PolyTreeD.t))

  (* PolyTree64 Methods *)

  let polytree64_get =
    foreign
      "clipper_polytree64_get"
      (ptr PolyTree64.t @-> size_t @-> returning (ptr PolyTree64.t))

  let polytree64_add_child =
    foreign
      "clipper_polytree64_add_child"
      (ptr PolyTree64.t @-> ptr Path64.t @-> returning (ptr PolyTree64.t))

  let polytree64_clear =
    foreign "clipper_polytree64_clear" (ptr PolyTree64.t @-> returning void)

  let polytree64_count =
    foreign "clipper_polytree64_count" (ptr PolyTree64.t @-> returning size_t)

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

  (* PolyTreeD Methods *)

  let polytreed_get =
    foreign
      "clipper_polytreed_get"
      (ptr PolyTreeD.t @-> size_t @-> returning (ptr PolyTreeD.t))

  let polytreed_add_child =
    foreign
      "clipper_polytreed_add_child"
      (ptr PolyTreeD.t @-> ptr Path64.t @-> returning (ptr PolyTreeD.t))

  let polytreed_clear =
    foreign "clipper_polytreed_clear" (ptr PolyTreeD.t @-> returning void)

  let polytreed_count =
    foreign "clipper_polytreed_count" (ptr PolyTreeD.t @-> returning size_t)

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

  (* Rect Constructors *)

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

  (* Rect64 Methods *)

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
    foreign "clipper_rect64_scale" (ptr Rect64.t @-> double @-> returning void)

  let rect64_is_empty = foreign "clipper_rect64_is_empty" (ptr Rect64.t @-> returning bool)

  let rect64_intersects =
    foreign "clipper_rect64_intersects" (ptr Rect64.t @-> ptr Rect64.t @-> returning bool)

  (* RectD Methods *)

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
    foreign "clipper_rectd_scale" (ptr RectD.t @-> double @-> returning void)

  let rectd_is_empty = foreign "clipper_rectd_is_empty" (ptr RectD.t @-> returning bool)

  let rectd_intersects =
    foreign "clipper_rectd_intersects" (ptr RectD.t @-> ptr RectD.t @-> returning bool)

  (* Clipper Contsructors *)

  let clipper64 = foreign "clipper_clipper64" (ptr void @-> returning (ptr Clipper64.t))

  let clipperd =
    foreign "clipper_clipperd" (ptr void @-> int @-> returning (ptr ClipperD.t))

  (* Clipper64 Setters / Getters *)

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

  (* ClipperD Setters / Getters *)

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

  (* Clipper64 Methods *)

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

  (* ClipperD Methods *)

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

  (* memory size *)

  let path64_size = foreign "clipper_path64_size" (ptr Path64.t @-> returning size_t)
  let pathd_size = foreign "clipper_pathd_size" (ptr PathD.t @-> returning size_t)
  let paths64_size = foreign "clipper_paths64_size" (ptr Paths64.t @-> returning size_t)
  let pathsd_size = foreign "clipper_pathsd_size" (ptr PathsD.t @-> returning size_t)
  let rect64_size = foreign "clipper_rect64_size" (ptr Rect64.t @-> returning size_t)
  let rectd_size = foreign "clipper_rectd_size" (ptr RectD.t @-> returning size_t)

  let polytree64_size =
    foreign "clipper_polytree64_size" (ptr PolyTree64.t @-> returning size_t)

  let polytreed_size =
    foreign "clipper_polytreed_size" (ptr PolyTreeD.t @-> returning size_t)

  let clipper64_size =
    foreign "clipper_clipper64_size" (ptr Clipper64.t @-> returning size_t)

  let clipperd_size = foreign "clipper_clipperd_size" (ptr ClipperD.t @-> returning size_t)

  (* destruction *)

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

  (* pointer free + destruction *)

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
end
