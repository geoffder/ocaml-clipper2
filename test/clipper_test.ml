module C =
  Clipper.MakeD'
    (OCADml.V2)
    (struct
      type t = OCADml.Poly2.t

      let to_list t = t.OCADml.Poly2.outer :: t.holes

      let of_list = function
        | [] -> OCADml.Poly2.make []
        | outer :: holes -> OCADml.Poly2.make ~holes outer
    end)
    ((val Clipper.config ()))

module Ctup = Clipper.MakeD (struct
  type t = float * float

  let v x y = x, y
  let x t = fst t
  let y t = snd t
end)

module C64 = Clipper.Make64 (struct
  type t = int64 * int64

  let v x y = x, y
  let x t = fst t
  let y t = snd t
end)

let%test "path" =
  let n = 10000 in
  let pts =
    List.init n (fun i ->
        let i = Float.of_int i in
        OCADml.v2 i i )
  in
  let path = C.Path.of_list pts in
  Gc.full_major ();
  List.for_all2 OCADml.V2.equal pts (C.Path.to_list path)

let%test "tup_d" =
  let a = List.map OCADml.V2.to_tup OCADml.(Path2.square (v2 1. 1.)) in
  let b = List.map (fun (x, y) -> x +. 0.5, y +. 0.5) a in
  let subjects = Ctup.Paths.of_list [ a; b ] in
  ignore (Ctup.Paths.union subjects);
  true

let%test "tup_64" =
  let a =
    List.map Int64.(fun (x, y) -> of_int x, of_int y) [ 0, 0; 10, 0; 10, 10; 0, 10 ]
  in
  let b = List.map Int64.(fun (x, y) -> add x (of_int 5), add y (of_int 5)) a in
  ignore C64.Paths.(union @@ of_list [ a; b ]);
  true

let%test "decomp" =
  let open OCADml in
  let c1 = Path2.circle ~fn:32 10.
  and c2 = List.rev @@ Path2.circle ~fn:32 8.
  and c3 = Path2.circle ~fn:32 4.
  and c4 = List.rev @@ Path2.circle ~fn:32 2. in
  let cs =
    C.Paths.boolean_op
      ~op:`Union
      (C.Paths.of_list [ c1; c2; c3; c4 ])
      (C.Paths.of_list [])
  in
  let open OSCADml in
  match C.Paths.to_polys cs with
  | [ p1; p2 ] ->
    let s1 = Scad.of_poly2 p1
    and s2 = Scad.of_poly2 p2 in
    Scad.(union [ (color Color.Red) s1; s2 ])
    |> Scad.to_file "/home/geoff/git/ocaml-clipper2/decomp.scad";
    true
  | _ -> false
