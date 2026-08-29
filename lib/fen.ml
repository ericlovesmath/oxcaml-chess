open Core

let fail fmt = Printf.ksprintf (fun message -> failwith ("Fen: " ^ message)) fmt

(* TODO: I understand that I can't unbox an [or_null], maybe I need a refactor... *)
let square_char board square =
  match Board.kind_at board square, Board.color_at board square with
  | This kind, This color -> Some (Piece.to_char #{ color; kind })
  | _ -> None
;;

let rank_to_string board rank =
  List.init 8 ~f:(fun file -> square_char board (Square.create ~rank ~file))
  |> List.group ~break:(fun a b -> not (Option.is_none a && Option.is_none b))
  |> List.map ~f:(function
    | None :: _ as empties -> Int.to_string (List.length empties)
    | pieces -> String.of_list (List.filter_opt pieces))
  |> String.concat
;;

let of_board board =
  List.init 8 ~f:(rank_to_string board) |> List.rev |> String.concat ~sep:"/"
;;

let color_kind_of_char c =
  match Piece.Kind.of_char c with
  | Some kind -> (if Char.is_uppercase c then Piece.Color.White else Black), kind
  | None -> fail "%C is not a piece letter" c
;;

(* One rank field expanded to one entry per file *)
let expand_rank field =
  String.to_list field
  |> List.concat_map ~f:(fun c ->
    match Char.get_digit c with
    | Some 0 -> fail "a run of empty squares cannot be 0"
    | Some empties -> List.init empties ~f:(fun _ -> None)
    | None -> [ Some (color_kind_of_char c) ])
;;

let to_board_exn s =
  (* TODO: Use some kind of layout polymorphism so I don't have to rewrite all these *)
  let fields = String.split s ~on:'/' in
  if List.length fields <> 8 then fail "expected eight ranks, got %d" (List.length fields);
  let placements =
    List.concat_mapi fields ~f:(fun i field ->
      let rank = 7 - i in
      let files = expand_rank field in
      if List.length files <> 8
      then fail "rank %d has %d files" (rank + 1) (List.length files);
      List.filter_mapi files ~f:(fun file ->
        Option.map ~f:(fun (color, kind) -> color, kind, Square.create ~rank ~file)))
  in
  let rec place board = function
    | [] -> board
    | (color, kind, square) :: rest ->
      place (Board.toggle_piece board #{ color; kind } square) rest
  in
  let board = place Board.empty placements in
  Board.invariant board;
  board
;;

let of_position pos =
  String.concat
    ~sep:" "
    [ of_board (Position.board pos)
    ; Piece.Color.to_string (Position.to_move pos)
    ; Castling.to_string (Position.castling pos)
    ; (match Position.en_passant pos with
       | This square -> Square.to_string square
       | Null -> "-")
    ; Int.to_string (Position.halfmove_clock pos)
    ; Int.to_string (Position.fullmove_number pos)
    ]
;;

let castling_of_string s =
  match Castling.of_string s with
  | Some castling -> castling
  | None -> fail "bad castling rights %S" s
;;

let en_passant_of_string = function
  | "-" -> Null
  | s ->
    let square =
      match Square.of_string s with
      | Some square -> square
      | None -> fail "bad en passant square %S" s
    in
    This square
;;

let int_of_string name s =
  match Int.of_string_opt s with
  | Some n -> n
  | None -> fail "bad %s %S" name s
;;

let to_position s =
  try
    match String.split s ~on:' ' with
    | [ placement; side; rights; target; halfmove; fullmove ] ->
      Ok
        (Position.create_exn
           ~board:(to_board_exn placement)
           ~to_move:(Piece.Color.of_string side)
           ~castling:(castling_of_string rights)
           ~en_passant:(en_passant_of_string target)
           ~halfmove_clock:(int_of_string "halfmove clock" halfmove)
           ~fullmove_number:(int_of_string "fullmove number" fullmove))
    | fields -> fail "expected six fields, got %d" (List.length fields)
  with
  | Failure message -> Error message
;;

let startpos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
let kiwipete = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R"
let endgame = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8"

let%expect_test "FEN and board" =
  List.iter [ startpos; kiwipete; endgame ] ~f:(fun fen ->
    print_endline (Board.to_string (to_board_exn fen));
    printf "%s\n\n" fen);
  [%expect
    {|
    8 r n b q k b n r
    7 p p p p p p p p
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . . . . .
    3 . . . . . . . .
    2 P P P P P P P P
    1 R N B Q K B N R
      a b c d e f g h

    rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR

    8 r . . . k . . r
    7 p . p p q p b .
    6 b n . . p n p .
    5 . . . P N . . .
    4 . p . . P . . .
    3 . . N . . Q . p
    2 P P P B B P P P
    1 R . . . K . . R
      a b c d e f g h

    r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R

    8 . . . . . . . .
    7 . . p . . . . .
    6 . . . p . . . .
    5 K P . . . . . r
    4 . R . . . p . k
    3 . . . . . . . .
    2 . . . . P . P .
    1 . . . . . . . .
      a b c d e f g h

    8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8
    |}]
;;

let%expect_test "round trip tests" =
  List.iter [ startpos; kiwipete; endgame; "8/8/8/8/8/8/8/8" ] ~f:(fun fen ->
    printf "%b %s\n" (String.equal fen (of_board (to_board_exn fen))) fen);
  [%expect
    {|
    true rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR
    true r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R
    true 8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8
    true 8/8/8/8/8/8/8/8
  |}]
;;

let%expect_test "malformed FEN tests" =
  List.iter
    [ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP"
    ; "8/8/8/8/8/8/8/8/8"
    ; "rnbqkbnr/ppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
    ; "rnbqkbnrb/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
    ; "rnbqkbnr/pppppppp/08/8/8/8/PPPPPPPP/RNBQKBNR"
    ; "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNX"
    ; ""
    ]
    ~f:(fun fen ->
      try printf "accepted %s\n" (of_board (to_board_exn fen)) with
      | Failure msg -> print_endline msg);
  [%expect
    {|
    Fen: expected eight ranks, got 7
    Fen: expected eight ranks, got 9
    Fen: rank 7 has 7 files
    Fen: rank 8 has 9 files
    Fen: a run of empty squares cannot be 0
    Fen: 'X' is not a piece letter
    Fen: expected eight ranks, got 1
    |}]
;;

let%expect_test "FEN of starting position" =
  print_endline (of_position Position.start);
  printf
    "matches start: %b\n"
    (String.equal (of_position Position.start) (startpos ^ " w KQkq - 0 1"));
  [%expect
    {|
    rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
    matches start: true
    |}]
;;

let%expect_test "invalid records" =
  List.iter
    [ startpos ^ " w KQkq - 0"
    ; startpos ^ " w KQkq - 0 1 x"
    ; startpos ^ " x KQkq - 0 1"
    ; startpos ^ " w KQkqx - 0 1"
    ; startpos ^ " w KQkq e9 0 1"
    ; startpos ^ " w KQkq - x 1"
    ; startpos ^ " w KQkq - 0 x"
    ; "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP w KQkq - 0 1"
    ; "4k3/8/8/8/8/8/8/4K3 w KQkq - 0 1"
    ]
    ~f:(fun fen ->
      match to_position fen with
      | Ok position -> printf "ACCEPTED %s\n" (of_position position)
      | Error message -> print_endline message);
  [%expect
    {|
    Fen: expected six fields, got 5
    Fen: expected six fields, got 7
    Piece.Color: invalid color x
    Fen: bad castling rights "KQkqx"
    Fen: bad en passant square "e9"
    Fen: bad halfmove clock "x"
    Fen: bad fullmove number "x"
    Fen: expected eight ranks, got 7
    Position: castling right K needs R on h1
    |}]
;;
