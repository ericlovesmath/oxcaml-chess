open Core
open Chess_primitives
open Chess_rules
module B = Bitboard

[@@@ocamlformat "disable"]

(* [Simple evaluation tables](https://chessprogramming.org/Simplified_Evaluation_Function),
   written from the perspective of white *)

let pawn_table =
  [:   0;   0;   0;   0;   0;   0;   0;   0
   ;  50;  50;  50;  50;  50;  50;  50;  50
   ;  10;  10;  20;  30;  30;  20;  10;  10
   ;   5;   5;  10;  25;  25;  10;   5;   5
   ;   0;   0;   0;  20;  20;   0;   0;   0
   ;   5;  -5; -10;   0;   0; -10;  -5;   5
   ;   5;  10;  10; -20; -20;  10;  10;   5
   ;   0;   0;   0;   0;   0;   0;   0;   0
  :]
;;

let knight_table =
  [: -50; -40; -30; -30; -30; -30; -40; -50
   ; -40; -20;   0;   0;   0;   0; -20; -40
   ; -30;   0;  10;  15;  15;  10;   0; -30
   ; -30;   5;  15;  20;  20;  15;   5; -30
   ; -30;   0;  15;  20;  20;  15;   0; -30
   ; -30;   5;  10;  15;  15;  10;   5; -30
   ; -40; -20;   0;   5;   5;   0; -20; -40
   ; -50; -40; -30; -30; -30; -30; -40; -50
  :]
;;

let bishop_table =
  [: -20; -10; -10; -10; -10; -10; -10; -20
   ; -10;   0;   0;   0;   0;   0;   0; -10
   ; -10;   0;   5;  10;  10;   5;   0; -10
   ; -10;   5;   5;  10;  10;   5;   5; -10
   ; -10;   0;  10;  10;  10;  10;   0; -10
   ; -10;  10;  10;  10;  10;  10;  10; -10
   ; -10;   5;   0;   0;   0;   0;   5; -10
   ; -20; -10; -10; -10; -10; -10; -10; -20
  :]
;;

let rook_table =
  [:   0;   0;   0;   0;   0;   0;   0;   0
   ;   5;  10;  10;  10;  10;  10;  10;   5
   ;  -5;   0;   0;   0;   0;   0;   0;  -5
   ;  -5;   0;   0;   0;   0;   0;   0;  -5
   ;  -5;   0;   0;   0;   0;   0;   0;  -5
   ;  -5;   0;   0;   0;   0;   0;   0;  -5
   ;  -5;   0;   0;   0;   0;   0;   0;  -5
   ;   0;   0;   0;   5;   5;   0;   0;   0
  :]
;;

let queen_table =
  [: -20; -10; -10;  -5;  -5; -10; -10; -20
   ; -10;   0;   0;   0;   0;   0;   0; -10
   ; -10;   0;   5;   5;   5;   5;   0; -10
   ;  -5;   0;   5;   5;   5;   5;   0;  -5
   ;   0;   0;   5;   5;   5;   5;   0;  -5
   ; -10;   5;   5;   5;   5;   5;   0; -10
   ; -10;   0;   5;   0;   0;   0;   0; -10
   ; -20; -10; -10;  -5;  -5; -10; -10; -20
  :]
;;

(* TODO: Should only be used for middle game *)
let king_table =
  [: -30; -40; -40; -50; -50; -40; -40; -30
   ; -30; -40; -40; -50; -50; -40; -40; -30
   ; -30; -40; -40; -50; -50; -40; -40; -30
   ; -30; -40; -40; -50; -50; -40; -40; -30
   ; -20; -30; -30; -40; -40; -30; -30; -20
   ; -10; -20; -20; -20; -20; -20; -20; -10
   ;  20;  20;   0;   0;   0;  20;  20;  20
   ;  20;  30;  10;   0;   0;  10;  30;  20
  :]
;;
[@@@ocamlformat "enable"]

let piece_value (piece : Piece.Kind.t) : int =
  match piece with
  | Pawn -> 100
  | Knight -> 300
  | Bishop -> 330
  | Rook -> 500
  | Queen -> 900
  | King -> 2000 (* The two kings cancel so the value is arbitrarily large *)
;;

let with_value piece table = Iarray.map table ~f:(fun v -> piece_value piece + v)

let tables =
  [: with_value Pawn pawn_table
   ; with_value Knight knight_table
   ; with_value Bishop bishop_table
   ; with_value Rook rook_table
   ; with_value Queen queen_table
   ; with_value King king_table
  :]
;;

(* [lxor 56] flips the rank to index one for black's eval tables *)
let table_index (color : Piece.Color.t) (square : Square.t) =
  match color with
  | White -> (square :> int) lxor 56
  | Black -> (square :> int)
;;

let value_to_owner (piece : Piece.t) square =
  let table = Iarray.unsafe_get tables (Piece.Kind.to_index piece.#kind) in
  Iarray.unsafe_get table (table_index piece.#color square)
;;

let rec sum_squares piece squares acc =
  if B.is_empty squares
  then acc
  else (
    let square = B.lowest_square squares in
    sum_squares piece (B.remove_lowest squares) (acc + value_to_owner piece square))
;;

(* NOTE: not [List.fold], whose [~f] would capture [board] and [color]. That closure is a
   heap allocation, and [score] is [zero_alloc strict]. *)
let rec sum_kinds board color kinds acc =
  match kinds with
  | [] -> acc
  | kind :: rest ->
    let piece : Piece.t = #{ color; kind } in
    sum_kinds board color rest (acc + sum_squares piece (Board.piece_board board piece) 0)
;;

let sum_pieces board color = sum_kinds board color Piece.Kind.all 0

let score (position @ local) =
  let board = Position.board position in
  let us = Position.to_move position in
  sum_pieces board us - sum_pieces board (Piece.Color.flip us)
;;

let delta (position @ local) move =
  let color = Position.to_move position in
  let moved = Move.moved move in
  let arrived = Or_null.value (Move.promotion move) ~default:moved in
  let taken =
    match Move.captured move with
    | Null -> 0
    | This kind ->
      value_to_owner #{ color = Piece.Color.flip color; kind } (Move.captured_square move)
  in
  let[@inline] ours kind square = value_to_owner #{ color; kind } square in
  let rook =
    match Move.kind move with
    | Castle ->
      let #(rook_from, rook_to) = Move.castle_rook move in
      ours Rook rook_to - ours Rook rook_from
    | Normal | Double_push | En_passant -> 0
  in
  ours arrived (Move.to_ move) - ours moved (Move.from move) + taken + rook
;;

let%expect_test "delta vs score check" =
  let row (kind, fen, uci) =
    let position = Fen.to_position_exn fen in
    match Movegen.find position uci with
    | Null -> failwithf "%s is not legal in %s" uci fen ()
    | This move ->
      let before = score position in
      let delta = delta position move in
      [%sexp
        { kind : string
        ; move = (Move.san move : string)
        ; before : int
        ; delta : int
        ; after = (-(before + delta) : int)
        }]
  in
  Test_positions.
    [ "quiet", startpos, "g1f3"
    ; "double push", startpos, "e2e4"
    ; "capture", kiwipete, "e2a6"
    ; "castle kingside", kiwipete, "e1g1"
    ; "castle queenside", kiwipete, "e1c1"
    ]
  |> List.map ~f:row
  |> Expectable.print;
  [%expect
    {|
    ┌──────────────────┬───────┬────────┬───────┬───────┐
    │ kind             │ move  │ before │ delta │ after │
    ├──────────────────┼───────┼────────┼───────┼───────┤
    │ quiet            │ Nf3   │   0    │  50   │  -50  │
    │ double push      │ e4    │   0    │  40   │  -40  │
    │ capture          │ Bxa6  │ 105    │ 310   │ -415  │
    │ castle kingside  │ O-O   │ 105    │  30   │ -135  │
    │ castle queenside │ O-O-O │ 105    │  15   │ -120  │
    └──────────────────┴───────┴────────┴───────┴───────┘
    |}]
;;
