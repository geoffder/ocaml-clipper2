open OCADml
open OSCADml

let () =
  let sq = Poly2.box ~center:true ~thickness:(v2 2. 2.) (v2 6. 6.)
  and circ = Path2.circle ~fn:32 1. in
  let a = Scad.of_mesh @@ Mesh.extrude ~height:1. sq
  and b =
    Scad.minkowski [ Scad.of_poly2 sq; Scad.of_path2 circ ]
    |> Scad.extrude ~height:1.
    |> Scad.ztrans 3.
  and c =
    C.minkowski_sum ~pattern:(C.path circ) (C.of_poly sq)
    |> C.to_polys
    |> List.map Scad.of_poly2
    |> Scad.union
    |> Scad.extrude ~height:1.
    |> Scad.ztrans (-3.)
  in
  Scad.to_file "minkowski.scad" (Scad.union [ a; b; c ])

let () =
  let sq =
    Poly2.box ~center:true ~thickness:(v2 2. 2.) (v2 6. 6.)
    |> Poly2.to_list
    |> List.map (List.map V2.to_tup)
    |> Clpr.PathsD.of_tups
  and pattern = Clpr.PathD.of_tups @@ List.map V2.to_tup @@ Path2.circle ~fn:32 1. in
  let ps = Clpr.PathsD.minkowski_sum ~pattern sq
  and w = Clpr.SvgWriter.make () in
  Clpr.SvgWriter.add_pathsd w ps;
  Result.get_ok @@ Clpr.SvgWriter.save w "minkowski_clpr.png"

let () =
  let sq = C.of_poly @@ Poly2.box ~center:true ~thickness:(v2 2. 2.) (v2 6. 6.)
  and pattern = C.path @@ Path2.circle ~fn:32 1. in
  let ps = C.minkowski_sum ~pattern sq in
  Result.get_ok @@ C.Svg.(write "minkowski.png" [ paint ~brush:(color ~alpha:1. Navy) ps ])
