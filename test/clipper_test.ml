module C =
  Clipper.MakeD' (OCADml.V2) (OCADml.Path2) (OCADml.Poly2) ((val Clipper.config ()))

module Ctup =
  Clipper.MakeD
    (struct
      type t = float * float

      let v x y = x, y
      let x t = fst t
      let y t = snd t
    end)
    ((val Clipper.config ()))

module C64 =
  Clipper.Make64'
    (struct
      type t = int64 * int64

      let v x y = x, y
      let x t = fst t
      let y t = snd t
    end)
    (struct
      type t = (int64 * int64) array

      let of_seq s = Array.of_seq s
      let to_seq a = Array.to_seq a
    end)
    (struct
      type t = (int64 * int64) array array

      let of_seq s = Array.of_seq @@ Seq.map Array.of_seq s
      let to_seq a = Seq.map Array.to_seq @@ Array.to_seq a
    end)
    ((val Clipper.config ()))

let%test "path" =
  let n = 10000 in
  let pts =
    List.init n (fun i ->
      let i = Float.of_int i in
      OCADml.v2 i i )
  in
  let path = C.path pts in
  Gc.full_major ();
  List.for_all2 OCADml.V2.equal pts (C.contour path)

let%test "tup_d" =
  let a = List.map OCADml.V2.to_tup OCADml.(Path2.square (v2 1. 1.)) in
  let b = List.map (fun (x, y) -> x +. 0.5, y +. 0.5) a in
  let subjects = Ctup.of_poly [ a; b ] in
  ignore (Ctup.union [ subjects ]);
  true

let%test "tup_64" =
  let a =
    Array.map Int64.(fun (x, y) -> of_int x, of_int y) [| 0, 0; 10, 0; 10, 10; 0, 10 |]
  in
  let b = Array.map Int64.(fun (x, y) -> add x (of_int 5), add y (of_int 5)) a in
  ignore C64.(union @@ [ path a; path b ]);
  true

let%test "decomp" =
  let open OCADml in
  let c1 = Path2.circle ~fn:32 10.
  and c2 = List.rev @@ Path2.circle ~fn:32 8.
  and c3 = Path2.circle ~fn:32 4.
  and c4 = List.rev @@ Path2.circle ~fn:32 2. in
  let cs = C.boolean_op ~op:`Union (C.paths [ c1; c2; c3; c4 ]) [] in
  match C.to_polys cs with
  | [ p1; p2 ] ->
    let a1 = Poly2.(area @@ of_list [ c1; c2 ])
    and a1' = Poly2.area p1
    and a2 = Poly2.(area @@ of_list [ c3; c4 ])
    and a2' = Poly2.area p2 in
    Math.approx ~eps:0.1 a1 a1' && Math.approx ~eps:0.1 a2 a2'
  | _ -> false
