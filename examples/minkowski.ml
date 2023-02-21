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
    C.of_poly sq
    |> C.minkowski_sum ~pattern:(C.path circ)
    |> C.to_polys
    |> List.map Scad.of_poly2
    |> Scad.union
    |> Scad.extrude ~height:1.
    |> Scad.ztrans (-3.)
  in
  Scad.to_file "minkowski.scad" (Scad.union [ a; b; c ])
