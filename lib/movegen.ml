open Core
module B = Bitboard
module D = Bitboard.Direction

(* TODO: I doubt this padding will slow it down, but check later *)

(** The theoretical maximum number of moves is 218, but we just pad *)
let max_moves = 255

(* NOTE: I love uniqueness so much thank you Ms. Street *)
module Movelist = struct
  type t =
    #{ moves : Move.t array
     ; start : int
     ; length : int
     }

  let dummy_move =
    let a1 = Square.unsafe_of_int 0 in
    Move.quiet ~moved:Pawn ~from:a1 ~to_:a1
  ;;

  let create () : t @ unique =
    #{ moves = Array.create ~len:max_moves dummy_move; start = 0; length = 0 }
  ;;

  let clear (t : t @ unique) : t @ unique = #{ t with start = 0; length = 0 }

  let push (t : t @ unique) (move : Move.t) : t @ unique =
    let #{ moves; start; length } = t in
    Array.set (borrow_ moves) length move;
    #{ moves; start; length = length + 1 }
  ;;

  let length t = t.#length - t.#start

  let pop t =
    if t.#start >= t.#length
    then #(Null, t)
    else #(This t.#moves.(t.#start), #{ t with start = t.#start + 1 })
  ;;
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

(** Trying Queen promotion first because... that seems reasonably better *)
let push_promotions ~captured ~from ~to_ moves =
  let moves = Movelist.push moves (Move.promote ~to_kind:Queen ~captured ~from ~to_) in
  let moves = Movelist.push moves (Move.promote ~to_kind:Rook ~captured ~from ~to_) in
  let moves = Movelist.push moves (Move.promote ~to_kind:Bishop ~captured ~from ~to_) in
  Movelist.push moves (Move.promote ~to_kind:Knight ~captured ~from ~to_)
;;

(* What a pawn's destination becomes *)
type pawn_becomes =
  | Step
  | Double
  | Passing

let pawn_move ~becomes ~captured ~from ~to_ =
  match becomes with
  | Step ->
    (match captured with
     | Null -> Move.quiet ~moved:Pawn ~from ~to_
     | This captured -> Move.capture ~moved:Pawn ~captured ~from ~to_)
  | Double -> Move.double_push ~from
  | Passing -> Move.en_passant ~from ~to_
;;

(* A pawn stepping up the board promotes on rank 8 and one stepping down on rank 1, so the
   rank follows from the direction of travel and need not be passed around. *)
let promoting ~delta to_ = Square.rank to_ = if delta > 0 then 7 else 0

let rec emit_pawn ~board ~becomes ~delta targets moves =
  if B.is_empty targets
  then moves
  else (
    let to_ = B.lowest_square targets in
    let from = Square.unsafe_of_int ((to_ :> int) - delta) in
    let captured = Board.kind_at board to_ in
    let moves =
      if promoting ~delta to_
      then push_promotions ~captured ~from ~to_ moves
      else Movelist.push moves (pawn_move ~becomes ~captured ~from ~to_)
    in
    emit_pawn ~board ~becomes ~delta (B.remove_lowest targets) moves)
;;

let emit_pawns ~board ~to_move ~en_passant moves =
  let pawns = B.(Board.kind_board board Pawn land Board.color_board board to_move) in
  let vacant = B.lnot (Board.occupancy board) in
  let them = Board.color_board board (Piece.Color.flip to_move) in
  let #(forward, staging) =
    match (to_move : Piece.Color.t) with
    | White -> #(D.North, 2)
    | Black -> #(D.South, 5)
  in
  let in_passing =
    match en_passant with
    | Null -> B.empty
    | This square -> B.of_square square
  in
  (* Captures *)
  let advanced = B.shift pawns forward in
  let east = B.shift advanced D.East
  and west = B.shift advanced D.West in
  (* Push twice and keep what lands rather than testing home ranks *)
  let pushed = B.(advanced land vacant) in
  let pushed_twice = B.(shift (pushed land rank_mask staging) forward land vacant) in
  let took_east = B.(east land them)
  and took_west = B.(west land them) in
  let passed_east = B.(east land in_passing)
  and passed_west = B.(west land in_passing) in
  let step = D.delta forward in
  let east_step = step + D.delta D.East
  and west_step = step + D.delta D.West in
  let moves = emit_pawn ~board ~becomes:Step ~delta:step pushed moves in
  let moves = emit_pawn ~board ~becomes:Double ~delta:(step * 2) pushed_twice moves in
  let moves = emit_pawn ~board ~becomes:Step ~delta:east_step took_east moves in
  let moves = emit_pawn ~board ~becomes:Step ~delta:west_step took_west moves in
  let moves = emit_pawn ~board ~becomes:Passing ~delta:east_step passed_east moves in
  emit_pawn ~board ~becomes:Passing ~delta:west_step passed_west moves
;;

let emit_castle ~board ~to_move ~castling ~side moves =
  let rank =
    match (to_move : Piece.Color.t) with
    | White -> 0
    | Black -> 7
  in
  let file f = Square.create ~rank ~file:f in
  let b = file 1
  and c = file 2
  and d = file 3
  and e = file 4
  and f = file 5
  and g = file 6 in
  let #(between, transit, final) =
    match (side : Castling.Side.t) with
    | Kingside -> #(B.(of_square f lor of_square g), f, g)
    | Queenside -> #(B.(of_square b lor of_square c lor of_square d), d, c)
  in
  (* TODO: So unergonomic for performance... maybe I can just force inline? *)
  let unattacked ~board a b c =
    let by = Piece.Color.flip to_move in
    (not (Attacks.is_attacked board a ~by))
    && (not (Attacks.is_attacked board b ~by))
    && not (Attacks.is_attacked board c ~by)
  in
  if Castling.mem castling to_move side
     && B.is_empty B.(between land Board.occupancy board)
     && unattacked ~board e transit final
  then Movelist.push moves (Move.castle ~color:to_move ~side)
  else moves
;;

let generate position moves =
  let board = Position.board position in
  let to_move = Position.to_move position in
  let moves = Movelist.clear moves in
  let en_passant = Position.en_passant position in
  let moves = emit_pawns ~board ~to_move ~en_passant moves in
  let moves = emit_kind ~board ~to_move ~kind:Knight moves in
  let moves = emit_kind ~board ~to_move ~kind:Bishop moves in
  let moves = emit_kind ~board ~to_move ~kind:Rook moves in
  let moves = emit_kind ~board ~to_move ~kind:Queen moves in
  let moves = emit_kind ~board ~to_move ~kind:King moves in
  let castling = Position.castling position in
  let moves = emit_castle ~board ~to_move ~castling ~side:Kingside moves in
  let moves = emit_castle ~board ~to_move ~castling ~side:Queenside moves in
  moves
;;

let rec scan moves token =
  match Movelist.pop moves with
  | #(Null, _) -> Null
  | #(This move, rest) ->
    if String.equal (Move.to_string move) token then This move else scan rest token
;;

let find (position @ local) token = scan (generate position (Movelist.create ())) token

let rec to_list moves acc =
  match Movelist.pop moves with
  | #(Null, _) -> List.rev acc
  | #(This move, rest) -> to_list rest (move :: acc)
;;

(** Visualization of [fen] position and available moves *)
let show fen =
  let position =
    match Fen.to_position fen with
    | Ok position -> position
    | Error message -> failwith message
  in
  let moves = generate position (Movelist.create ()) in
  let moves = to_list moves [] in
  List.iter2_exn
    (String.split_lines (Board.to_string (Position.board position)))
    (String.split_lines (Move.diagram ~color:(Position.to_move position) moves))
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
  show "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
  [%expect
    {|
      8 r n b q k b n r   8 . . . . . . . .
      7 p p p p p p p p   7 . . . . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 * * * * * * * *
      3 . . . . . . . .   3 * * * * * * * *
      2 P P P P P P P P   2 P P P P P P P P
      1 R N B Q K B N R   1 . N . . . . N .
        a b c d e f g h     a b c d e f g h
    20 moves: Na3 Nc3 Nf3 Nh3 a3 a4 b3 b4 c3 c4 d3 d4 e3 e4 f3 f4 g3 g4 h3 h4
    |}]
;;

let%expect_test "ray stopped by capture or own piece" =
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

let%expect_test "pawn pushes and captures" =
  show "4k3/8/8/8/1p2p3/P1P5/1P6/4K3 w - - 0 1";
  [%expect
    {|
      8 . . . . k . . .   8 . . . . . . . .
      7 . . . . . . . .   7 . . . . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . p . . p . . .   4 * * * . . . . .
      3 P . P . . . . .   3 P * P . . . . .
      2 . P . . . . . .   2 . P . * * * . .
      1 . . . . K . . .   1 . . . * K * . .
        a b c d e f g h     a b c d e f g h
    10 moves: Kd1 Kd2 Ke2 Kf1 Kf2 a4 axb4 b3 c4 cxb4
    |}]
;;

let%expect_test "blockers ahead of a pawn" =
  show "4k3/8/8/8/2n5/1n6/PPPP4/4K3 w - - 0 1";
  [%expect
    {|
      8 . . . . k . . .   8 . . . . . . . .
      7 . . . . . . . .   7 . . . . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . n . . . . .   4 * . . * . . . .
      3 . n . . . . . .   3 * * * * . . . .
      2 P P P P . . . .   2 P . P P * * . .
      1 . . . . K . . .   1 . . . * K * . .
        a b c d e f g h     a b c d e f g h
    11 moves: Kd1 Ke2 Kf1 Kf2 a3 a4 axb3 c3 cxb3 d3 d4
    |}]
;;

let%expect_test "en passant" =
  show "4k3/8/8/2pP4/8/8/8/4K3 w - c6 0 1";
  [%expect
    {|
      8 . . . . k . . .   8 . . . . . . . .
      7 . . . . . . . .   7 . . . . . . . .
      6 . . . . . . . .   6 . . * * . . . .
      5 . . p P . . . .   5 . . x P . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 . . . . . . . .   2 . . . * * * . .
      1 . . . . K . . .   1 . . . * K * . .
        a b c d e f g h     a b c d e f g h
    7 moves: Kd1 Kd2 Ke2 Kf1 Kf2 d6 dxc6
    |}]
;;

let%expect_test "promotions, by pushing and by capturing" =
  show "1n1nk3/2P5/8/8/8/8/8/4K3 w - - 0 1";
  [%expect
    {|
      8 . n . n k . . .   8 . * * * . . . .
      7 . . P . . . . .   7 . . P . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 . . . . . . . .   2 . . . * * * . .
      1 . . . . K . . .   1 . . . * K * . .
        a b c d e f g h     a b c d e f g h
    17 moves: Kd1 Kd2 Ke2 Kf1 Kf2 c8=B c8=N c8=Q c8=R cxb8=B cxb8=N cxb8=Q cxb8=R cxd8=B cxd8=N cxd8=Q cxd8=R
    |}]
;;

let%expect_test "a blocked promotion push still leaves the captures" =
  show "1nbnk3/2P5/8/8/8/8/8/4K3 w - - 0 1";
  [%expect
    {|
      8 . n b n k . . .   8 . * . * . . . .
      7 . . P . . . . .   7 . . P . . . . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 . . . . . . . .   2 . . . * * * . .
      1 . . . . K . . .   1 . . . * K * . .
        a b c d e f g h     a b c d e f g h
    13 moves: Kd1 Kd2 Ke2 Kf1 Kf2 cxb8=B cxb8=N cxb8=Q cxb8=R cxd8=B cxd8=N cxd8=Q cxd8=R
    |}]
;;

let%expect_test "black promotion" =
  show "4k3/8/8/8/8/8/2p5/4K3 b - - 0 1";
  [%expect
    {|
      8 . . . . k . . .   8 . . . * k * . .
      7 . . . . . . . .   7 . . . * * * . .
      6 . . . . . . . .   6 . . . . . . . .
      5 . . . . . . . .   5 . . . . . . . .
      4 . . . . . . . .   4 . . . . . . . .
      3 . . . . . . . .   3 . . . . . . . .
      2 . . p . . . . .   2 . . p . . . . .
      1 . . . . K . . .   1 . . * . . . . .
        a b c d e f g h     a b c d e f g h
    9 moves: Kd7 Kd8 Ke7 Kf7 Kf8 c1=B c1=N c1=Q c1=R
    |}]
;;

let%expect_test "castling when the back rank is clear" =
  show "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1";
  [%expect
    {|
      8 r . . . k . . r   8 * . . . . . . *
      7 . . . . . . . .   7 * . . . . . . *
      6 . . . . . . . .   6 * . . . . . . *
      5 . . . . . . . .   5 * . . . . . . *
      4 . . . . . . . .   4 * . . . . . . *
      3 . . . . . . . .   3 * . . . . . . *
      2 . . . . . . . .   2 * . . * * * . *
      1 R . . . K . . R   1 R * * * K * * R
        a b c d e f g h     a b c d e f g h
    26 moves: Kd1 Kd2 Ke2 Kf1 Kf2 O-O O-O-O Ra2 Ra3 Ra4 Ra5 Ra6 Ra7 Rb1 Rc1 Rd1 Rf1 Rg1 Rh2 Rh3 Rh4 Rh5 Rh6 Rh7 Rxa8 Rxh8
    |}]
;;

let%expect_test "castling blocked by a piece and check" =
  show "r3k2r/8/8/8/8/8/8/R2QK2R w KQkq - 0 1";
  show "r3k2r/8/8/8/8/8/4r3/R3K2R w KQkq - 0 1";
  [%expect
    {|
      8 r . . . k . . r   8 * . . * . . . *
      7 . . . . . . . .   7 * . . * . . . *
      6 . . . . . . . .   6 * . . * . . . *
      5 . . . . . . . .   5 * . . * . . . *
      4 . . . . . . . .   4 * . . * . . * *
      3 . . . . . . . .   3 * * . * . * . *
      2 . . . . . . . .   2 * . * * * * . *
      1 R . . Q K . . R   1 R * * Q K * * R
        a b c d e f g h     a b c d e f g h
    39 moves: Kd2 Ke2 Kf1 Kf2 O-O Qa4 Qb1 Qb3 Qc1 Qc2 Qd2 Qd3 Qd4 Qd5 Qd6 Qd7 Qd8 Qe2 Qf3 Qg4 Qh5 Ra2 Ra3 Ra4 Ra5 Ra6 Ra7 Rb1 Rc1 Rf1 Rg1 Rh2 Rh3 Rh4 Rh5 Rh6 Rh7 Rxa8 Rxh8
      8 r . . . k . . r   8 * . . . . . . *
      7 . . . . . . . .   7 * . . . . . . *
      6 . . . . . . . .   6 * . . . . . . *
      5 . . . . . . . .   5 * . . . . . . *
      4 . . . . . . . .   4 * . . . . . . *
      3 . . . . . . . .   3 * . . . . . . *
      2 . . . . r . . .   2 * . . * * * . *
      1 R . . . K . . R   1 R * * * K * * R
        a b c d e f g h     a b c d e f g h
    24 moves: Kd1 Kd2 Kf1 Kf2 Kxe2 Ra2 Ra3 Ra4 Ra5 Ra6 Ra7 Rb1 Rc1 Rd1 Rf1 Rg1 Rh2 Rh3 Rh4 Rh5 Rh6 Rh7 Rxa8 Rxh8
    |}]
;;

let%expect_test "rook crossing attacked square" =
  show "r3k2r/8/8/8/8/8/5r2/R3K2R w KQkq - 0 1";
  show "r3k2r/8/8/8/8/8/1r6/R3K2R w KQkq - 0 1";
  [%expect
    {|
      8 r . . . k . . r   8 * . . . . . . *
      7 . . . . . . . .   7 * . . . . . . *
      6 . . . . . . . .   6 * . . . . . . *
      5 . . . . . . . .   5 * . . . . . . *
      4 . . . . . . . .   4 * . . . . . . *
      3 . . . . . . . .   3 * . . . . . . *
      2 . . . . . r . .   2 * . . * * * . *
      1 R . . . K . . R   1 R * * * K * * R
        a b c d e f g h     a b c d e f g h
    25 moves: Kd1 Kd2 Ke2 Kf1 Kxf2 O-O-O Ra2 Ra3 Ra4 Ra5 Ra6 Ra7 Rb1 Rc1 Rd1 Rf1 Rg1 Rh2 Rh3 Rh4 Rh5 Rh6 Rh7 Rxa8 Rxh8
      8 r . . . k . . r   8 * . . . . . . *
      7 . . . . . . . .   7 * . . . . . . *
      6 . . . . . . . .   6 * . . . . . . *
      5 . . . . . . . .   5 * . . . . . . *
      4 . . . . . . . .   4 * . . . . . . *
      3 . . . . . . . .   3 * . . . . . . *
      2 . r . . . . . .   2 * . . * * * . *
      1 R . . . K . . R   1 R * * * K * * R
        a b c d e f g h     a b c d e f g h
    26 moves: Kd1 Kd2 Ke2 Kf1 Kf2 O-O O-O-O Ra2 Ra3 Ra4 Ra5 Ra6 Ra7 Rb1 Rc1 Rd1 Rf1 Rg1 Rh2 Rh3 Rh4 Rh5 Rh6 Rh7 Rxa8 Rxh8
    |}]
;;

let%expect_test "position with given up castling rights" =
  show "r3k2r/8/8/8/8/8/8/R3K2R w kq - 0 1";
  [%expect
    {|
      8 r . . . k . . r   8 * . . . . . . *
      7 . . . . . . . .   7 * . . . . . . *
      6 . . . . . . . .   6 * . . . . . . *
      5 . . . . . . . .   5 * . . . . . . *
      4 . . . . . . . .   4 * . . . . . . *
      3 . . . . . . . .   3 * . . . . . . *
      2 . . . . . . . .   2 * . . * * * . *
      1 R . . . K . . R   1 R * * * K * * R
        a b c d e f g h     a b c d e f g h
    24 moves: Kd1 Kd2 Ke2 Kf1 Kf2 Ra2 Ra3 Ra4 Ra5 Ra6 Ra7 Rb1 Rc1 Rd1 Rf1 Rg1 Rh2 Rh3 Rh4 Rh5 Rh6 Rh7 Rxa8 Rxh8
    |}]
;;
