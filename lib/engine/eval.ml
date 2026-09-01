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

let with_value value table = Iarray.map table ~f:(fun placement -> value + placement)

let tables =
  [: with_value 100 pawn_table
   ; with_value 300 knight_table
   ; with_value 330 bishop_table
   ; with_value 500 rook_table
   ; with_value 900 queen_table
   ; with_value 2000 king_table (* The two kings cancel so the value is arbitrary *)
  :]
;;

(* [lxor 56] flips the rank to index one for black's eval tables *)
let table_index (color : Piece.Color.t) (square : Square.t) =
  match color with
  | White -> (square :> int) lxor 56
  | Black -> (square :> int)
;;

let rec sum_squares table ~color squares acc =
  if B.is_empty squares
  then acc
  else (
    let entry = Iarray.unsafe_get table (table_index color (B.lowest_square squares)) in
    sum_squares table ~color (B.remove_lowest squares) (acc + entry))
;;

(* TODO: Test List.sum [@@kind ??] maybe? *)
let rec sum_kinds board color kinds acc =
  match kinds with
  | [] -> acc
  | kind :: rest ->
    let table = Iarray.unsafe_get tables (Piece.Kind.to_index kind) in
    let squares = Board.piece_board board #{ color; kind } in
    sum_kinds board color rest (acc + sum_squares table ~color squares 0)
;;

let evaluate_side board color = sum_kinds board color Piece.Kind.all 0

let evaluate (position @ local) =
  let board = Position.board position in
  let white = evaluate_side board White in
  let black = evaluate_side board Black in
  match Position.to_move position with
  | White -> white - black
  | Black -> black - white
;;
