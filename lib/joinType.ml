type t = C.Types.JoinType.t =
  | Square
  | Round
  | Miter

let make = function
  | `Square -> Square, 2.
  | `Round -> Round, 2.
  | `Miter None -> Miter, 2.
  | `Miter (Some limit) ->
    if limit >= 2. then Miter, limit else invalid_arg "Miter limit can be no less than 2."
