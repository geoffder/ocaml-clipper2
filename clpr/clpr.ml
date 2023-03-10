type rectd = C.Types.RectD.t Ctypes_static.ptr
type pathd = C.Types.PathD.t Ctypes_static.ptr
type pathsd = C.Types.PathsD.t Ctypes_static.ptr
type polytreed = C.Types.PolyTreeD.t Ctypes_static.ptr
type rect64 = C.Types.Rect64.t Ctypes_static.ptr
type path64 = C.Types.Path64.t Ctypes_static.ptr
type paths64 = C.Types.Paths64.t Ctypes_static.ptr
type polytree64 = C.Types.PolyTree64.t Ctypes_static.ptr

include ConfigTypes
module PointD = PointD
module RectD = RectD
module PathD = PathD
module PathsD = PathsD
module PolyTreeD = PolyTreeD
module ClipperD = ClipperD
module Point64 = Point64
module Rect64 = Rect64
module Path64 = Path64
module Paths64 = Paths64
module PolyTree64 = PolyTree64
module Clipper64 = Clipper64
module Offset = Offset
module SvgWriter = SvgWriter
module SvgReader = SvgReader
