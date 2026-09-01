open Core
module Movelist = Movegen.Movelist

(* TODO: Quiescence Search *)
(* TODO: Deeper depth after move ordering *)
let start_depth = 4

(** Arbitrarily high score for checkmates, but faster checkmates are better *)
let mate ply = 100_000 - ply

(** NOTE: Using a large number rather than [Int.max_value] as negation overflows *)
let inf = 1_000_000

(** If no legal moves, we assume mate if the side to move is in check, stalemate otherwise *)
let terminal (position @ local) ~ply = if Position.in_check position then -mate ply else 0

(* Negamax with alpha-beta pruning, returns the best score and the move *)
let rec negamax (position @ local) ~depth ~alpha ~beta =
  if depth = 0
  then
    (* TODO: We do not check terminal checks very well, which is a problem... need to fix
       with some kind of [unmove] maybe? *)
    #(Eval.evaluate position, Null)
  else (
    let moves = Movegen.generate position (Movelist.create ()) in
    match best_move position moves ~depth ~alpha ~beta #(-inf, Null) with
    (* Nothing beat [-inf], so nothing was legal *)
    | #(_, Null) -> #(terminal position ~ply:(start_depth - depth), Null)
    | #(_, This _) as best -> best)

and best_move (position @ local) moves ~depth ~alpha ~beta best =
  let #(best_score, _) = best in
  let bound = if best_score > alpha then best_score else alpha in
  if bound >= beta
  then best
  else (
    match Movelist.pop moves with
    | #(Null, _) -> best
    | #(This move, rest) ->
      let after = Position.make_move position move in
      let best =
        if Position.mover_in_check after
        then best
        else (
          let #(child, _) =
            negamax after ~depth:(depth - 1) ~alpha:(-beta) ~beta:(-bound)
          in
          let score = -child in
          if score > best_score then #(score, This move) else best)
      in
      best_move position rest ~depth ~alpha ~beta best)
;;

let search (position @ local) =
  let #(_, move) = negamax position ~depth:start_depth ~alpha:(-inf) ~beta:inf in
  move
;;

let%expect_test "trivial search tests" =
  let test fen =
    match search (Fen.to_position_exn fen) with
    | Null -> print_endline "none"
    | This move -> print_endline (Move.san move)
  in
  (* Mate in 1 *)
  test "6k1/5ppp/8/8/8/8/8/R3K3 w Q - 0 1";
  (* Hanging Queen *)
  test "4k3/8/8/3q4/4B3/8/8/4K3 w - - 0 1";
  (* stalemate *)
  test "7k/5Q2/6K1/8/8/8/8/8 b - - 0 1";
  (* checkmate *)
  test "R5k1/5ppp/8/8/8/8/8/6K1 b - - 0 1";
  [%expect {|
    Ra8
    Bxd5
    none
    none
    |}]
;;
