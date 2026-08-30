open Core
module B = Bitboard

(* TODO: I doubt this padding will slow it down, but check later *)

(** The theoretical maximum number of moves is 218, but we just pad *)
let max_moves = 255

(* NOTE: I love uniqueness so much thank you Ms. Street *)
module Movelist = struct
  type t =
    #{ moves : Move.t array
     ; length : int
     }

  let dummy_move =
    let a1 = Square.unsafe_of_int 0 in
    Move.quiet ~moved:Pawn ~from:a1 ~to_:a1
  ;;

  let create () : t @ unique =
    #{ moves = Array.create ~len:max_moves dummy_move; length = 0 }
  ;;

  let clear (t : t @ unique) : t @ unique = #{ t with length = 0 }

  let push (t : t @ unique) (move : Move.t) : t @ unique =
    let #{ moves; length } = t in
    Array.set (borrow_ moves) length move;
    #{ moves; length = length + 1 }
  ;;

  let length t = t.#length
  let get t i = t.#moves.(i)
end

(** NOTE: One loop over the destinations rather than separate quiet and capture passes *)
let rec emit_targets ~board ~moved ~from targets moves =
  if B.is_empty targets
  then moves
  else (
    let to_ = B.lowest_square targets in
    let move =
      match Board.kind_at board to_ with
      | Null -> Move.quiet ~moved ~from ~to_
      | This captured -> Move.capture ~moved ~captured ~from ~to_
    in
    emit_targets ~board ~moved ~from (B.remove_lowest targets) (Movelist.push moves move))
;;

let attacks_of ~occupancy ~kind square =
  match (kind : Piece.Kind.t) with
  | Knight -> Attacks.knight square
  | Bishop -> Attacks.bishop ~occupancy square
  | Rook -> Attacks.rook ~occupancy square
  | Queen -> Attacks.queen ~occupancy square
  | King -> Attacks.king square
  | Pawn -> B.empty
;;

let rec emit_pieces ~board ~to_move ~kind pieces moves =
  if B.is_empty pieces
  then moves
  else (
    let from = B.lowest_square pieces in
    let attacks = attacks_of ~occupancy:(Board.occupancy board) ~kind from in
    let targets = B.(attacks - Board.color_board board to_move) in
    let moves = emit_targets ~board ~moved:kind ~from targets moves in
    emit_pieces ~board ~to_move ~kind (B.remove_lowest pieces) moves)
;;

let emit_kind ~board ~to_move ~kind moves =
  let pieces = B.(Board.kind_board board kind land Board.color_board board to_move) in
  emit_pieces ~board ~to_move ~kind pieces moves
;;

(** NOTE: [exclusive mutable] (https://oxcaml.org/documentation/uniqueness/pitfalls/) is a
    feature that is not implemented yet... but once it is, we can use a locally mutable
    unique value to avoid having to thread [moves]. Hopefull this is patched in soon. *)
let generate position moves =
  let board = Position.board position in
  let to_move = Position.to_move position in
  let moves = Movelist.clear moves in
  let moves = emit_kind ~board ~to_move ~kind:Knight moves in
  let moves = emit_kind ~board ~to_move ~kind:Bishop moves in
  let moves = emit_kind ~board ~to_move ~kind:Rook moves in
  let moves = emit_kind ~board ~to_move ~kind:Queen moves in
  let moves = emit_kind ~board ~to_move ~kind:King moves in
  moves
;;

(** Visualization of [fen] position and available moves *)
let show fen =
  let position =
    match Fen.to_position fen with
    | Ok position -> position
    | Error message -> failwith message
  in
  let moves = generate position (Movelist.create ()) in
  let moves = List.init (Movelist.length moves) ~f:(Movelist.get moves) in
  List.iter2_exn
    (String.split_lines (Board.to_string (Position.board position)))
    (String.split_lines (Move.diagram moves))
    ~f:(printf "  %-17s   %s\n");
  moves
  |> List.map ~f:Move.san
  |> List.sort ~compare:String.compare
  |> List.cons (Int.to_string (List.length moves) ^ " moves:")
  |> String.concat ~sep:" "
  |> print_endline
;;

let%expect_test "a knight in the corner and the king next to it" =
  show "4k3/8/8/8/8/8/8/N3K3 w - - 0 1";
  [%expect
    {|
      8 . . . . k . . .   8 . . . . . . . .
      7 . . . . . . . .   7 . . . . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . * . . . . . .
      2 . . . . . . . .   2 . . * * * * . .
      1 N . . . K . . .   1 N . . * K * . .
        a b c d e f g h     a b c d e f g h
    7 moves: Kd1 Kd2 Ke2 Kf1 Kf2 Nb3 Nc2
    |}]
;;

let%expect_test "own pieces are not targets" =
  (** TODO: Pawns are not implemented yet lol *)
  show "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
  [%expect
    {|
      8 r n b q k b n r   8 . . . . . . . .
      7 p p p p p p p p   7 . . . . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 * . * . . * . *
      2 P P P P P P P P   2 . . . . . . . .
      1 R N B Q K B N R   1 . N . . . . N .
        a b c d e f g h     a b c d e f g h
    4 moves: Na3 Nc3 Nf3 Nh3
    |}]
;;

let%expect_test "ray stoped by capture or own piece" =
  show "4k3/8/8/3r4/8/3Q4/3P4/K7 w - - 0 1";
  [%expect
    {|
      8 . . . . k . . .   8 . . . . . . . .
      7 . . . . . . . .   7 . . . . . . . *
      6 . . . . . . . .   6 * . . . . . * .
      5 . . . r . . . .   5 . * . * . * . .
      4 . . . . . . . .   4 . . * * * . . .
      3 . . . Q . . . .   3 * * * Q * * * *
      2 . . . P . . . .   2 * * * . * . . .
      1 K . . . . . . .   1 K * . . . * . .
        a b c d e f g h     a b c d e f g h
    23 moves: Ka2 Kb1 Kb2 Qa3 Qa6 Qb1 Qb3 Qb5 Qc2 Qc3 Qc4 Qd4 Qe2 Qe3 Qe4 Qf1 Qf3 Qf5 Qg3 Qg6 Qh3 Qh7 Qxd5
    |}]
;;
