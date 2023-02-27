open OCADml

let () =
  let s =
    let circ = Path2.circle ~fn:32 50. in
    let f i = Path2.xtrans (Int.to_float i *. 75.) circ in
    C.union [ C.paths @@ List.init 5 f ]
  in
  Result.get_ok @@ C.Svg.(write "union.png" [ solution s ])

let () =
  let sq = C.path @@ Path2.square (v2 5. 5.)
  and circ = C.path @@ Path2.circle ~fn:32 5. in
  let s = C.intersect [ circ; sq ] in
  Result.get_ok @@ C.Svg.(write "intersection.png" [ subject sq; clip circ; solution s ])

let () =
  let sq = C.of_poly @@ Poly2.box ~center:true ~thickness:(v2 2. 2.) (v2 6. 6.)
  and circ = C.of_poly @@ Poly2.circle ~fn:32 5. in
  let s = C.sub circ sq in
  Result.get_ok @@ C.Svg.(write "difference.png" [ subject circ; clip sq; solution s ])
