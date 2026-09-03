open Core
open Chess_primitives
open Chess_rules

(* TODO: Quiescence Search *)
(* TODO: Killer moves and a history table, scored above quiets and below captures *)

type result =
  #{ score : int
   ; move : Move.t or_null
   ; nodes : int
   }

(** Arbitrarily high score for checkmates, but faster checkmates are better *)
let mate ply = 100_000 - ply

(** NOTE: Using a large number rather than [Int.max_value] as negation overflows *)
let inf = 1_000_000

(** If no legal moves, we assume mate if the side to move is in check, stalemate otherwise *)
let terminal (position @ local) ~ply = if Position.in_check position then -mate ply else 0

type node =
  #{ depth : int
   ; ply : int
   ; alpha : int
   ; beta : int
   }

(** Node one ply deeper with the turn flipped *)
let child_of node ~bound =
  #{ depth = node.#depth - 1; ply = node.#ply + 1; alpha = -node.#beta; beta = -bound }
;;

(** Folds a searched [child] into [best] *)
let improve best move ~child =
  let nodes = best.#nodes + child.#nodes in
  let score = -child.#score in
  if score > best.#score
  then #{ score; move = This move; nodes }
  else #{ best with nodes }
;;

(* Negamax with alpha-beta pruning, returns the best score and the move *)
let rec negamax (position @ local) node =
  if node.#depth = 0
  then
    (* TODO: We do not check terminal checks very well, which is a problem... need to fix
       with some kind of [unmove] maybe? *)
    #{ score = Eval.evaluate position; move = Null; nodes = 1 }
  else (
    let moves = Movelist.sorted (Movegen.generate position) ~score:Move_order.score in
    let start = #{ score = -inf; move = Null; nodes = 1 } in
    let best = best_move position moves node ~i:0 start in
    match best.#move with
    (* Nothing beat [-inf], so nothing was legal *)
    | Null -> #{ best with score = terminal position ~ply:node.#ply }
    | This _ -> best)

and best_move (position @ local) moves node ~i best =
  let bound = Int.max best.#score node.#alpha in
  if bound >= node.#beta || i >= Movelist.length moves
  then best
  else (
    let move = Movelist.get moves i in
    let after = Position.make_move position move in
    let best =
      if Position.mover_in_check after
      then best
      else (
        let child = negamax after (child_of node ~bound) in
        improve best move ~child)
    in
    best_move position moves node ~i:(i + 1) best)
;;

let search (position @ local) ~depth =
  negamax position #{ depth; ply = 0; alpha = -inf; beta = inf }
;;

let%expect_test "trivial search tests" =
  let test fen =
    match (search (Fen.to_position_exn fen) ~depth:4).#move with
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
