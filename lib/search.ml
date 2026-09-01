open Core

(** TODO: This is temporary, we just return the first legal move *)
let search (position @ local) =
  let moves = Movegen.generate position (Movegen.Movelist.create ()) in
  let n = Movegen.Movelist.length moves in
  let mutable i = 0 in
  let mutable legal = Null in
  while Or_null.is_null legal && i < n do
    let move = Movegen.Movelist.get moves i in
    let after = Position.make_move position move in
    if not (Position.mover_in_check after) then legal <- This move;
    i <- i + 1
  done;
  legal
;;
