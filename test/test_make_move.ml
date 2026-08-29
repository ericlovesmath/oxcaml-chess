open Core
open Oxcaml_chess

let sq = Square.of_string_exn
let startpos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

let position_exn fen =
  match Fen.to_position fen with
  | Ok position -> position
  | Error message -> failwith message
;;

(* All tests are also allocation tests! *)
let apply before move = exclave_
  Expect_test_helpers_core.require_no_allocation_local (fun () -> exclave_
    Position.make_move before move)
;;

(* Before and after, side by side *)
let show fen move =
  let rows board fen =
    String.split_lines (Board.to_string board)
    @ [ String.split fen ~on:' ' |> List.tl_exn |> String.concat ~sep:" " ]
  in
  let before = position_exn fen in
  let after = apply before move in
  Position.invariant after;
  printf "%s\n" (Move.to_string move);
  List.iter2_exn
    (rows (Position.board before) fen)
    (rows (Position.board after) (Fen.of_position after))
    ~f:(printf "  %-17s   %s\n");
  printf "\n"
;;

let%expect_test "simple moves tested" =
  show startpos (Move.quiet ~moved:Knight ~from:(sq "g1") ~to_:(sq "f3"));
  show startpos (Move.double_push ~from:(sq "e2"));
  show
    "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2"
    (Move.capture ~moved:Pawn ~captured:Pawn ~from:(sq "e4") ~to_:(sq "d5"));
  [%expect
    {|
    g1f3
      8 r n b q k b n r   8 r n b q k b n r
      7 p p p p p p p p   7 p p p p p p p p
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . N . .
      2 P P P P P P P P   2 P P P P P P P P
      1 R N B Q K B N R   1 R N B Q K B . R
        a b c d e f g h     a b c d e f g h
      w KQkq - 0 1        b KQkq - 1 1

    e2e4
      8 r n b q k b n r   8 r n b q k b n r
      7 p p p p p p p p   7 p p p p p p p p
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . P . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 P P P P P P P P   2 P P P P . P P P
      1 R N B Q K B N R   1 R N B Q K B N R
        a b c d e f g h     a b c d e f g h
      w KQkq - 0 1        b KQkq e3 0 1

    e4d5
      8 r n b q k b n r   8 r n b q k b n r
      7 p p p . p p p p   7 p p p . p p p p
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . p . . . .   5 . . . P . . . .
      4 . . . . P . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 P P P P . P P P   2 P P P P . P P P
      1 R N B Q K B N R   1 R N B Q K B N R
        a b c d e f g h     a b c d e f g h
      w KQkq d6 0 2       b KQkq - 0 2
    |}]
;;

let%expect_test "the fullmove number advances only after black" =
  show startpos (Move.quiet ~moved:Knight ~from:(sq "b1") ~to_:(sq "c3"));
  show
    "rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/R1BQKBNR b KQkq - 1 1"
    (Move.quiet ~moved:Knight ~from:(sq "b8") ~to_:(sq "c6"));
  [%expect
    {|
    b1c3
      8 r n b q k b n r   8 r n b q k b n r
      7 p p p p p p p p   7 p p p p p p p p
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . N . . . . .
      2 P P P P P P P P   2 P P P P P P P P
      1 R N B Q K B N R   1 R . B Q K B N R
        a b c d e f g h     a b c d e f g h
      w KQkq - 0 1        b KQkq - 1 1

    b8c6
      8 r n b q k b n r   8 r . b q k b n r
      7 p p p p p p p p   7 p p p p p p p p
      6 . . . . . . . .   6 . . n . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . N . . . . .   3 . . N . . . . .
      2 P P P P P P P P   2 P P P P P P P P
      1 R . B Q K B N R   1 R . B Q K B N R
        a b c d e f g h     a b c d e f g h
      b KQkq - 1 1        w KQkq - 2 2
    |}]
;;

let%expect_test "halfmove clock" =
  let fen = "r1bqkbnr/pppppppp/2n5/8/8/2N5/PPPPPPPP/R1BQKBNR w KQkq - 7 5" in
  show fen (Move.quiet ~moved:Knight ~from:(sq "c3") ~to_:(sq "b1"));
  show fen (Move.double_push ~from:(sq "a2"));
  [%expect
    {|
    c3b1
      8 r . b q k b n r   8 r . b q k b n r
      7 p p p p p p p p   7 p p p p p p p p
      6 . . n . . . . .   6 . . n . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . N . . . . .   3 . . . . . . . .
      2 P P P P P P P P   2 P P P P P P P P
      1 R . B Q K B N R   1 R N B Q K B N R
        a b c d e f g h     a b c d e f g h
      w KQkq - 7 5        b KQkq - 8 5

    a2a4
      8 r . b q k b n r   8 r . b q k b n r
      7 p p p p p p p p   7 p p p p p p p p
      6 . . n . . . . .   6 . . n . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 P . . . . . . .
      3 . . N . . . . .   3 . . N . . . . .
      2 P P P P P P P P   2 . P P P P P P P
      1 R . B Q K B N R   1 R . B Q K B N R
        a b c d e f g h     a b c d e f g h
      w KQkq - 7 5        b KQkq a3 0 5
    |}]
;;

let%expect_test "a double push sets the en passant square" =
  show startpos (Move.double_push ~from:(sq "d2"));
  show
    "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"
    (Move.quiet ~moved:Knight ~from:(sq "g1") ~to_:(sq "f3"));
  [%expect
    {|
    d2d4
      8 r n b q k b n r   8 r n b q k b n r
      7 p p p p p p p p   7 p p p p p p p p
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . P . . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 P P P P P P P P   2 P P P . P P P P
      1 R N B Q K B N R   1 R N B Q K B N R
        a b c d e f g h     a b c d e f g h
      w KQkq - 0 1        b KQkq d3 0 1

    g1f3
      8 r n b q k b n r   8 r n b q k b n r
      7 p p p . p p p p   7 p p p . p p p p
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . p P . . .   5 . . . p P . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . N . .
      2 P P P P . P P P   2 P P P P . P P P
      1 R N B Q K B N R   1 R N B Q K B . R
        a b c d e f g h     a b c d e f g h
      w KQkq d6 0 3       b KQkq - 1 3
    |}]
;;

let%expect_test "castling moves the rook" =
  let white = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1" in
  let black = "r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1" in
  show white (Move.castle ~color:White ~side:Kingside);
  show white (Move.castle ~color:White ~side:Queenside);
  show black (Move.castle ~color:Black ~side:Kingside);
  show black (Move.castle ~color:Black ~side:Queenside);
  [%expect
    {|
    e1g1
      8 r . . . k . . r   8 r . . . k . . r
      7 . . . . . . . .   7 . . . . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 . . . . . . . .   2 . . . . . . . .
      1 R . . . K . . R   1 R . . . . R K .
        a b c d e f g h     a b c d e f g h
      w KQkq - 0 1        b kq - 1 1

    e1c1
      8 r . . . k . . r   8 r . . . k . . r
      7 . . . . . . . .   7 . . . . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 . . . . . . . .   2 . . . . . . . .
      1 R . . . K . . R   1 . . K R . . . R
        a b c d e f g h     a b c d e f g h
      w KQkq - 0 1        b kq - 1 1

    e8g8
      8 r . . . k . . r   8 r . . . . r k .
      7 . . . . . . . .   7 . . . . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 . . . . . . . .   2 . . . . . . . .
      1 R . . . K . . R   1 R . . . K . . R
        a b c d e f g h     a b c d e f g h
      b KQkq - 0 1        w KQ - 1 2

    e8c8
      8 r . . . k . . r   8 . . k r . . . r
      7 . . . . . . . .   7 . . . . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 . . . . . . . .   2 . . . . . . . .
      1 R . . . K . . R   1 R . . . K . . R
        a b c d e f g h     a b c d e f g h
      b KQkq - 0 1        w KQ - 1 2
    |}]
;;

let%expect_test "revoke castling rights when the king/rook moves, or rook is captured" =
  let fen = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1" in
  show fen (Move.quiet ~moved:King ~from:(sq "e1") ~to_:(sq "e2"));
  show fen (Move.quiet ~moved:Rook ~from:(sq "a1") ~to_:(sq "b1"));
  show
    "r3k2r/7Q/8/8/8/8/8/R3K2R w KQkq - 0 1"
    (Move.capture ~moved:Queen ~captured:Rook ~from:(sq "h7") ~to_:(sq "h8"));
  [%expect
    {|
    e1e2
      8 r . . . k . . r   8 r . . . k . . r
      7 . . . . . . . .   7 . . . . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 . . . . . . . .   2 . . . . K . . .
      1 R . . . K . . R   1 R . . . . . . R
        a b c d e f g h     a b c d e f g h
      w KQkq - 0 1        b kq - 1 1

    a1b1
      8 r . . . k . . r   8 r . . . k . . r
      7 . . . . . . . .   7 . . . . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 . . . . . . . .   2 . . . . . . . .
      1 R . . . K . . R   1 . R . . K . . R
        a b c d e f g h     a b c d e f g h
      w KQkq - 0 1        b Kkq - 1 1

    h7h8
      8 r . . . k . . r   8 r . . . k . . Q
      7 . . . . . . . Q   7 . . . . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 . . . . . . . .   2 . . . . . . . .
      1 R . . . K . . R   1 R . . . K . . R
        a b c d e f g h     a b c d e f g h
      w KQkq - 0 1        b KQq - 0 1
    |}]
;;
