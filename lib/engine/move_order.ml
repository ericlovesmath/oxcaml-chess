open Core
open Chess_primitives

let aggressor_value (piece : Piece.Kind.t) : int =
  match piece with
  | Pawn -> 1
  | Knight -> 2
  | Bishop -> 3
  | Rook -> 4
  | Queen -> 5
  | King -> 0
;;

let score move =
  let capture =
    match Move.captured move with
    | Null -> 0
    | This victim -> Eval.piece_value victim - aggressor_value (Move.moved move)
  in
  let promotion =
    match Move.promotion move with
    | Null -> 0
    | This kind -> Eval.piece_value kind - Eval.piece_value Pawn
  in
  capture + promotion
;;

let%expect_test "move order basic test" =
  let sq = Square.of_string_exn in
  let moves =
    [ Move.quiet ~moved:Knight ~from:(sq "g1") ~to_:(sq "f3")
    ; Move.capture ~moved:Queen ~captured:Pawn ~from:(sq "d1") ~to_:(sq "d7")
    ; Move.capture ~moved:Pawn ~captured:Queen ~from:(sq "e4") ~to_:(sq "d5")
    ; Move.capture ~moved:Rook ~captured:Queen ~from:(sq "d1") ~to_:(sq "d8")
    ; Move.capture ~moved:King ~captured:Pawn ~from:(sq "e1") ~to_:(sq "e2")
    ; Move.en_passant ~from:(sq "e5") ~to_:(sq "d6")
    ; Move.promote ~to_kind:Queen ~captured:Null ~from:(sq "a7") ~to_:(sq "a8")
    ; Move.castle ~color:White ~side:Kingside
    ]
  in
  moves
  |> List.sort ~compare:(fun a b -> Int.descending (score a) (score b))
  |> List.iter ~f:(fun move -> printf "%s (%d)\n" (Move.san move) (score move));
  [%expect
    {|
    exd5 (899)
    Rxd8 (896)
    a8=Q (800)
    Kxe2 (100)
    exd6 (99)
    Qxd7 (95)
    Nf3 (0)
    O-O (0)
    |}]
;;
