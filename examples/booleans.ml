open OCADml

let () =
  let sq = C.of_poly @@ Poly2.box ~center:true ~thickness:(v2 2. 2.) (v2 6. 6.)
  and circ = C.of_poly @@ Poly2.circle ~fn:32 5. in
  let ps = C.sub circ sq in
  Result.get_ok @@ C.Svg.(write "difference.png" [ subject circ; clip sq; solution ps ])
