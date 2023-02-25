open Conv

type t = C.Types.SvgWriter.t Ctypes_static.ptr

let size = C.Funcs.svgwriter_size () |> size_to_int
let destruct t = C.Funcs.destruct_svgwriter t

let alloc () =
  let finalise = Mem.finaliser C.Types.SvgWriter.t destruct in
  let buf = Mem.allocate_buf ~finalise size in
  buf, Ctypes_static.(Ctypes.coerce (ptr void) (ptr C.Types.SvgWriter.t) buf)

let make ?(precision = 2) () =
  let buf, t = alloc () in
  let _ = C.Funcs.svgwriter buf precision in
  t

let fill_rule t =
  match C.Funcs.svgwriter_fill_rule t with
  | FillRule.EvenOdd -> `EvenOdd
  | NonZero -> `NonZero
  | Positive -> `Positive
  | Negative -> `Negative

let set_coords_style
  ?(font_name = "Verdana")
  ?(font_color = 0xFF000000)
  ?(font_size = 11)
  t
  =
  let name = string_to_ptr Ctypes.char font_name
  and clr = Unsigned.UInt32.of_int font_color
  and sz = Unsigned.UInt32.of_int font_size in
  C.Funcs.svgwriter_set_coords_style t name clr sz

let add_text ?(font_color = 0xFF000000) ?(font_size = 11) ~x ~y t text =
  let txt = string_to_ptr Ctypes.char text
  and clr = Unsigned.UInt32.of_int font_color
  and sz = Unsigned.UInt32.of_int font_size in
  C.Funcs.svgwriter_add_text t txt clr sz x y

(* TODO: make convenient color variant that translates to colour as well as an
    arbitrary option. Should brush and pen be grouped in a record? Would make
    subject/clip/solution defaults simpler. *)

let subj_brush_clr = 0x1800009C
let subj_pen_clr = 0xCCB3B3DA
let clip_brush_clr = 0x129C0000
let clip_pen_clr = 0xCCFFA07A
let soln_pen_brush_clr = 0x4466FF66

let add_p'
  ?(closed = true)
  ?(fill_rule = `NonZero)
  ?(show_coords = false)
  ?(brush_color = subj_brush_clr)
  ?(pen_color = subj_pen_clr)
  ?(pen_width = 0.8)
  f
  t
  p
  =
  let fr = FillRule.make fill_rule
  and brush = Unsigned.UInt32.of_int brush_color
  and pen = Unsigned.UInt32.of_int pen_color in
  f t p (not closed) fr brush pen pen_width show_coords

let add_path64 ?closed ?fill_rule ?show_coords ?brush_color ?pen_color ?pen_width t p =
  let f = C.Funcs.svgwriter_add_path64 in
  add_p' ?closed ?fill_rule ?show_coords ?brush_color ?pen_color ?pen_width f t p

let add_pathd ?closed ?fill_rule ?show_coords ?brush_color ?pen_color ?pen_width t p =
  let f = C.Funcs.svgwriter_add_pathd in
  add_p' ?closed ?fill_rule ?show_coords ?brush_color ?pen_color ?pen_width f t p

let add_paths64 ?closed ?fill_rule ?show_coords ?brush_color ?pen_color ?pen_width t p =
  let f = C.Funcs.svgwriter_add_paths64 in
  add_p' ?closed ?fill_rule ?show_coords ?brush_color ?pen_color ?pen_width f t p

let add_pathsd ?closed ?fill_rule ?show_coords ?brush_color ?pen_color ?pen_width t p =
  let f = C.Funcs.svgwriter_add_pathsd in
  add_p' ?closed ?fill_rule ?show_coords ?brush_color ?pen_color ?pen_width f t p

let save ?(max_width = 0) ?(max_height = 0) ?(margin = 0) t filename =
  let name = string_to_ptr Ctypes.char filename in
  if C.Funcs.svgwriter_save_to_file t name max_width max_height margin
  then Ok ()
  else Error (Printf.sprintf "Failed to write svg to %s." filename)

let clear t = C.Funcs.svgwriter_clear t
