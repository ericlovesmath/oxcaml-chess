open Core
open Chess_primitives

type t =
  { board : Board.t
  ; to_move : Piece.Color.t
  ; castling : Castling.t
  ; en_passant : Square.t or_null
  ; halfmove_clock : int
  ; fullmove_number : int
  }

(* TODO: Maybe I just expose [t] and also make the fields mutable *)
let board (t @ local) = t.board
let to_move (t @ local) = t.to_move
let castling (t @ local) = t.castling
let en_passant (t @ local) = t.en_passant
let halfmove_clock (t @ local) = t.halfmove_clock
let fullmove_number (t @ local) = t.fullmove_number
let fail fmt = Printf.ksprintf (fun message -> failwith ("Position: " ^ message)) fmt

let check_en_passant board (to_move : Piece.Color.t) square =
  let target_rank, pawn_rank, pawn_color =
    match to_move with
    | White -> 5, 4, Piece.Color.Black
    | Black -> 2, 3, White
  in
  let name = Square.to_string square in
  if Square.rank square <> target_rank
  then fail "en passant square %s is on the wrong rank" name;
  if Bitboard.mem (Board.occupancy board) square
  then fail "en passant square %s is occupied" name;
  let pawn = Square.create ~rank:pawn_rank ~file:(Square.file square) in
  if not
       (Bitboard.mem (Board.piece_board board #{ color = pawn_color; kind = Pawn }) pawn)
  then fail "no pawn on %s to be captured en passant" (Square.to_string pawn)
;;

(* NOTE: Castling doesn't check if the king/rook has already moved *)
let check_castling board castling =
  let%with.tilde color = List.iter Piece.Color.all in
  let%with.tilde side = List.iter Castling.Side.all in
  let needs kind file =
    let rank =
      match color with
      | White -> 0
      | Black -> 7
    in
    let square = Square.create ~rank ~file in
    let piece : Piece.t = #{ color; kind } in
    if not (Bitboard.mem (Board.piece_board board piece) square)
    then
      fail
        "castling right %s needs %c on %s"
        Castling.(to_string (add none color side))
        (Piece.to_char piece)
        (Square.to_string square)
  in
  if Castling.mem castling color side
  then (
    needs King 4;
    needs
      Rook
      (match side with
       | Kingside -> 7
       | Queenside -> 0))
;;

let invariant (t @ local) =
  (* Bound out of the local record *)
  let local_ board = t.board in
  let local_ to_move = t.to_move in
  Board.invariant board;
  List.iter Piece.Color.all ~f:(fun color ->
    let kings = Bitboard.count (Board.piece_board board #{ color; kind = King }) in
    if kings <> 1
    then fail "%c has %d kings" (Piece.to_char #{ color; kind = King }) kings);
  let back_ranks = Bitboard.(rank_mask 0 lor rank_mask 7) in
  if not (Bitboard.is_empty Bitboard.(Board.kind_board board Pawn land back_ranks))
  then fail "a pawn is on rank 1 or rank 8";
  if t.halfmove_clock < 0 then fail "halfmove clock is negative";
  if t.fullmove_number < 1 then fail "fullmove number is below 1";
  if Attacks.in_check board (Piece.Color.flip to_move)
  then fail "the side that just moved left its king in check";
  (match t.en_passant with
   | Null -> ()
   | This square -> check_en_passant board to_move square);
  check_castling board t.castling
;;

let in_check (t @ local) = Attacks.in_check t.board t.to_move
let mover_in_check (t @ local) = Attacks.in_check t.board (Piece.Color.flip t.to_move)

(* TODO: Expose an unchecked version? *)
let create_exn ~board ~to_move ~castling ~en_passant ~halfmove_clock ~fullmove_number =
  let t = { board; to_move; castling; en_passant; halfmove_clock; fullmove_number } in
  invariant t;
  t
;;

let start =
  create_exn
    ~board:Board.start
    ~to_move:White
    ~castling:Castling.all
    ~en_passant:Null
    ~halfmove_clock:0
    ~fullmove_number:1
;;

(* The square a double push passes over *)
let skipped_square ~from ~to_ =
  Square.create ~rank:((Square.rank from + Square.rank to_) / 2) ~file:(Square.file from)
;;

(* A move touching a corner (as origin or destination) removed castling rights there *)
let revoke_at castling square =
  match Square.rank square, Square.file square with
  | 0, 0 -> Castling.remove castling White Queenside
  | 0, 7 -> Castling.remove castling White Kingside
  | 7, 0 -> Castling.remove castling Black Queenside
  | 7, 7 -> Castling.remove castling Black Kingside
  | _, _ -> castling
;;

let make_move (t @ local) move =
  let us = t.to_move in
  let them = Piece.Color.flip us in
  let from = Move.from move in
  let to_ = Move.to_ move in
  let moved = Move.moved move in
  let captured_square =
    match Move.kind move with
    | En_passant -> Square.create ~rank:(Square.rank from) ~file:(Square.file to_)
    | Normal | Double_push | Castle -> to_
  in
  let board =
    match Move.captured move with
    | Null -> t.board
    | This kind -> Board.toggle_piece t.board #{ color = them; kind } captured_square
  in
  let board =
    match Move.promotion move with
    | Null -> Board.move_piece board #{ color = us; kind = moved } ~from ~to_
    | This promoted ->
      (* A promotion changes the piece's kind, so it cannot go through [move_piece] *)
      Board.toggle_piece board #{ color = us; kind = moved } from
      |> fun board -> Board.toggle_piece board #{ color = us; kind = promoted } to_
  in
  let board =
    match Move.kind move with
    | Castle ->
      (* The rook's origin and destination for a castle *)
      let #(rook_from, rook_to) =
        let rank = Square.rank to_ in
        if Square.file to_ = 6
        then #(Square.create ~rank ~file:7, Square.create ~rank ~file:5)
        else #(Square.create ~rank ~file:0, Square.create ~rank ~file:3)
      in
      Board.move_piece board #{ color = us; kind = Rook } ~from:rook_from ~to_:rook_to
    | Normal | Double_push | En_passant -> board
  in
  exclave_
  { board
  ; to_move = them
  ; castling =
      (let castling =
         match moved with
         | King -> Castling.remove_color t.castling us
         | Pawn | Knight | Bishop | Rook | Queen -> t.castling
       in
       revoke_at (revoke_at castling from) to_)
  ; en_passant =
      (match Move.kind move with
       | Double_push -> This (skipped_square ~from ~to_)
       | Normal | En_passant | Castle -> Null)
  ; halfmove_clock =
      (if Piece.Kind.equal moved Pawn || Move.is_capture move
       then 0
       else t.halfmove_clock + 1)
  ; fullmove_number =
      (t.fullmove_number
       +
       match us with
       | White -> 0
       | Black -> 1)
  }
;;

let color_name (color : Piece.Color.t) =
  match color with
  | White -> "White"
  | Black -> "Black"
;;

let en_passant_name = function
  | Null -> "-"
  | This square -> Square.to_string square
;;

let to_string (t @ local) =
  let annotations =
    [ color_name t.to_move ^ " to move"
    ; "castling " ^ Castling.to_string t.castling
    ; "en passant " ^ en_passant_name t.en_passant
    ; sprintf "halfmove %d" t.halfmove_clock
    ; sprintf "fullmove %d" t.fullmove_number
    ]
  in
  String.split_lines (Board.to_string t.board)
  |> List.mapi ~f:(fun i line ->
    match List.nth annotations i with
    | Some annotation -> sprintf "%s   %s" line annotation
    | None -> line)
  |> String.concat_lines
;;

let put board color kind square =
  Board.toggle_piece board #{ color; kind } (Square.of_string_exn square)
;;

let bare_kings = put (put Board.empty White King "e1") Black King "e8"

let%expect_test "start" =
  print_endline (to_string start);
  [%expect
    {|
    8 r n b q k b n r   White to move
    7 p p p p p p p p   castling KQkq
    6 . . . . . . . .   en passant -
    5 . . . . . . . .   halfmove 0
    4 . . . . . . . .   fullmove 1
    3 . . . . . . . .
    2 P P P P P P P P
    1 R N B Q K B N R
      a b c d e f g h
    |}]
;;

let%expect_test "invariant rejects illegal states" =
  let attempt
    name
    ~board
    ?(castling = Castling.none)
    ?(ep = Null)
    ?(half = 0)
    ?(full = 1)
    ()
    =
    match
      create_exn
        ~board
        ~to_move:White
        ~castling
        ~en_passant:ep
        ~halfmove_clock:half
        ~fullmove_number:full
    with
    | (_ : t) -> printf "%-22s accepted\n" name
    | exception Failure message -> printf "%-22s %s\n" name message
  in
  let sq s = This (Square.of_string_exn s) in
  let base = bare_kings in
  attempt "legal" ~board:base ();
  attempt "negative clock" ~board:base ~half:(-1) ();
  attempt "fullmove 0" ~board:base ~full:0 ();
  attempt "ep on wrong rank" ~board:base ~ep:(sq "e3") ();
  attempt "ep with no pawn" ~board:base ~ep:(sq "e6") ();
  attempt "castling with no rook" ~board:base ~castling:Castling.all ();
  attempt "two white kings" ~board:(put base White King "d1") ();
  attempt "pawn on rank 8" ~board:(put base White Pawn "a8") ();
  [%expect
    {|
    legal                  accepted
    negative clock         Position: halfmove clock is negative
    fullmove 0             Position: fullmove number is below 1
    ep on wrong rank       Position: en passant square e3 is on the wrong rank
    ep with no pawn        Position: no pawn on e5 to be captured en passant
    castling with no rook  Position: castling right K needs R on h1
    two white kings        Position: K has 2 kings
    pawn on rank 8         Position: a pawn is on rank 1 or rank 8
    |}]
;;
