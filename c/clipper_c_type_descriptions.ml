module Types (F : Cstubs.Types.TYPE) = struct
  open Ctypes
  open F

  module Clipper64 = struct
    type t = [ `Clipper64 ] structure

    let t : t typ = structure "ClipperClipper64"
  end

  module ClipperD = struct
    type t = [ `ClipperD ] structure

    let t : t typ = structure "ClipperClipperD"
  end

  module Offset = struct
    type t = [ `Offset ] structure

    let t : t typ = structure "ClipperClipperOffset"
  end

  module SvgWriter = struct
    type t = [ `SvgWriter ] structure

    let t : t typ = structure "ClipperSvgWriter"
  end

  module SvgReader = struct
    type t = [ `SvgReader ] structure

    let t : t typ = structure "ClipperSvgReader"
  end

  module Path64 = struct
    type t = [ `Path64 ] structure

    let t : t typ = structure "ClipperPath64"
  end

  module PathD = struct
    type t = [ `PathD ] structure

    let t : t typ = structure "ClipperPathD"
  end

  module Paths64 = struct
    type t = [ `Paths64 ] structure

    let t : t typ = structure "ClipperPaths64"
  end

  module PathsD = struct
    type t = [ `PathsD ] structure

    let t : t typ = structure "ClipperPathsD"
  end

  module Rect64 = struct
    type t = [ `Rect64 ] structure

    let t : t typ = structure "ClipperRect64"
  end

  module RectD = struct
    type t = [ `RectD ] structure

    let t : t typ = structure "ClipperRectD"
  end

  module PolyTree64 = struct
    type t = [ `PolyTree64 ] structure

    let t : t typ = structure "ClipperPolyTree64"
  end

  module PolyTreeD = struct
    type t = [ `PolyTreeD ] structure

    let t : t typ = structure "ClipperPolyTreeD"
  end

  module Point64 = struct
    type t = [ `Point64 ] structure

    let t : t typ = structure "ClipperPoint64"
    let x = field t "x" int64_t
    let y = field t "y" int64_t
    let () = seal t
  end

  module PointD = struct
    type t = [ `PointD ] structure

    let t : t typ = structure "ClipperPointD"
    let x = field t "x" double
    let y = field t "y" double
    let () = seal t
  end

  module Rect64Struct = struct
    type t = [ `Rect64Struct ] structure

    let t : t typ = structure "ClipperRect64"
    let left = field t "left" int64_t
    let top = field t "top" int64_t
    let right = field t "right" int64_t
    let bottom = field t "bottom" int64_t
    let () = seal t
  end

  module RectDStruct = struct
    type t = [ `RectDStruct ] structure

    let t : t typ = structure "ClipperRectD"
    let left = field t "left" double
    let top = field t "top" double
    let right = field t "right" double
    let bottom = field t "bottom" double
    let () = seal t
  end

  module FillRule = struct
    let even_odd = constant "EVEN_ODD" int64_t
    let non_zero = constant "NON_ZERO" int64_t
    let positive = constant "POSITIVE" int64_t
    let negative = constant "NEGATIVE" int64_t

    type t =
      | EvenOdd
      | NonZero
      | Positive
      | Negative

    let t =
      enum
        "ClipperFillRule"
        [ EvenOdd, even_odd; NonZero, non_zero; Positive, positive; Negative, negative ]
        ~unexpected:(fun _ -> assert false)
  end

  module ClipType = struct
    let none = constant "NONE" int64_t
    let intersection = constant "INTERSECTION" int64_t
    let union = constant "UNION" int64_t
    let difference = constant "DIFFERENCE" int64_t
    let xor = constant "XOR" int64_t

    type t =
      | None
      | Intersection
      | Union
      | Difference
      | Xor

    let t =
      enum
        "ClipperClipType"
        [ None, none
        ; Intersection, intersection
        ; Union, union
        ; Difference, difference
        ; Xor, xor
        ]
        ~unexpected:(fun _ -> assert false)
  end

  module JoinType = struct
    let square_join = constant "SQUARE_JOIN" int64_t
    let round_join = constant "ROUND_JOIN" int64_t
    let miter_join = constant "MITER_JOIN" int64_t

    type t =
      | Square
      | Round
      | Miter

    let t =
      enum
        "ClipperJoinType"
        [ Square, square_join; Round, round_join; Miter, miter_join ]
        ~unexpected:(fun _ -> assert false)
  end

  module EndType = struct
    let polygon_end = constant "POLYGON_END" int64_t
    let joined_end = constant "JOINED_END" int64_t
    let butt_end = constant "BUTT_END" int64_t
    let square_end = constant "SQUARE_END" int64_t
    let round_end = constant "ROUND_END" int64_t

    type t =
      | Polygon
      | Joined
      | Butt
      | Square
      | Round

    let t =
      enum
        "ClipperEndType"
        [ Polygon, polygon_end
        ; Joined, joined_end
        ; Butt, butt_end
        ; Square, square_end
        ; Round, round_end
        ]
        ~unexpected:(fun _ -> assert false)
  end

  module PointInPolygon = struct
    let is_on = constant "IS_ON" int64_t
    let is_inside = constant "IS_INSIDE" int64_t
    let is_outside = constant "IS_OUTSIDE" int64_t

    type t =
      | IsOn
      | IsInside
      | IsOutside

    let t =
      enum
        "ClipperPointInPolygonResult"
        [ IsOn, is_on; IsInside, is_inside; IsOutside, is_outside ]
        ~unexpected:(fun _ -> assert false)
  end
end
