open Core

let startpos_fen = Fen.of_position Position.start

module Command = struct
  type t =
    | Uci
    | Is_ready
    | New_game
    | Set_position of
        { fen : string
        ; moves : string list
        }
    | Go
    | Display (* Custom command to dump debug information *)
    | Quit
    | Ignored

  let parse_position = function
    | "startpos" :: "moves" :: moves -> Set_position { fen = startpos_fen; moves }
    | "startpos" :: _ -> Set_position { fen = startpos_fen; moves = [] }
    | "fen" :: a :: b :: c :: d :: e :: f :: "moves" :: moves ->
      Set_position { fen = String.concat ~sep:" " [ a; b; c; d; e; f ]; moves }
    | "fen" :: a :: b :: c :: d :: e :: f :: _ ->
      Set_position { fen = String.concat ~sep:" " [ a; b; c; d; e; f ]; moves = [] }
    | _ -> Ignored
  ;;

  let parse line =
    match
      line
      |> String.split_on_chars ~on:[ ' '; '\t' ]
      |> List.filter ~f:(Fn.non String.is_empty)
    with
    | [ "uci" ] -> Uci
    | [ "isready" ] -> Is_ready
    | [ "ucinewgame" ] -> New_game
    | [ "disp" ] -> Display
    | [ "quit" ] -> Quit
    | "go" :: _ ->
      (* TODO: [go] currently ignores arguments *)
      Go
    | "position" :: tokens -> parse_position tokens
    | _ -> Ignored
  ;;
end

let rec apply (position @ local) moves =
  match moves with
  | [] -> Ok (Fen.of_position position)
  | token :: rest ->
    (match Movegen.find position token with
     | Null -> Error ("unrecognised move " ^ token)
     | This move ->
       (* TODO: Not tail recursive to avoid locality issues, probably a real fix exists *)
       let fen = apply (Position.make_move position move) rest in
       fen)
;;

let play fen moves =
  match Fen.to_position fen with
  | Ok position -> apply position moves
  | Error message -> Error ("bad fen: " ^ message)
;;

let bestmove ~choose fen =
  match Fen.to_position fen with
  | Error message -> "info string bad fen: " ^ message
  | Ok position ->
    (match choose position with
     | Null -> "bestmove 0000"
     | This move -> "bestmove " ^ Move.to_string move)
;;

let board fen =
  match Fen.to_position fen with
  | Error message -> [ "info string bad fen: " ^ message ]
  | Ok position -> String.split_lines (Position.to_string position)
;;

type outcome =
  | Stop
  | Continue of string * string list

let respond ~choose fen command =
  match (command : Command.t) with
  | Quit -> Stop
  | Ignored -> Continue (fen, [])
  | Uci -> Continue (fen, [ "id name OxCaml Chess"; "id author Eric Lee"; "uciok" ])
  | Is_ready -> Continue (fen, [ "readyok" ])
  | New_game -> Continue (startpos_fen, [])
  | Display -> Continue (fen, board fen)
  | Go -> Continue (fen, [ bestmove ~choose fen ])
  | Set_position { fen = requested; moves } ->
    (match play requested moves with
     | Ok fen -> Continue (fen, [])
     | Error message -> Continue (fen, [ "info string " ^ message ]))
;;

let rec loop ~read_line ~write_line ~choose fen =
  match read_line () with
  | None -> ()
  | Some line ->
    (match respond ~choose fen (Command.parse line) with
     | Stop -> ()
     | Continue (fen, output) ->
       List.iter output ~f:write_line;
       loop ~read_line ~write_line ~choose fen)
;;

let run ~read_line ~write_line ~choose = loop ~read_line ~write_line ~choose startpos_fen

module For_testing = struct
  let choose (_position @ local) =
    This
      (Move.quiet
         ~moved:Piece.Kind.Pawn
         ~from:(Square.of_string_exn "a1")
         ~to_:(Square.of_string_exn "a1"))
  ;;

  let simulate inputs =
    let remaining = ref inputs in
    let read_line () =
      match !remaining with
      | [] -> None
      | line :: rest ->
        remaining := rest;
        print_endline ("> " ^ line);
        Some line
    in
    let write_line line = print_endline ("< " ^ line) in
    run ~read_line ~write_line ~choose
  ;;
end

let%expect_test "basic simulation" =
  For_testing.simulate
    [ "uci"; "isready"; "position startpos moves e2e4 e7e5"; "disp"; "go"; "quit" ];
  [%expect
    {|
    > uci
    < id name OxCaml Chess
    < id author Eric Lee
    < uciok
    > isready
    < readyok
    > position startpos moves e2e4 e7e5
    > disp
    < 8 r n b q k b n r   White to move
    < 7 p p p p . p p p   castling KQkq
    < 6 . . . . . . . .   en passant e6
    < 5 . . . . p . . .   halfmove 0
    < 4 . . . . P . . .   fullmove 2
    < 3 . . . . . . . .
    < 2 P P P P . P P P
    < 1 R N B Q K B N R
    <   a b c d e f g h
    > go
    < bestmove a1a1
    > quit
    |}]
;;

let%expect_test "bad inputs" =
  For_testing.simulate
    [ "position startpos moves e2e4"
    ; "position fen not a valid fen at all"
    ; "position startpos moves e2e5"
    ; "setoption name Hash value 64"
    ; "stop"
    ; "kwyjibo"
    ; ""
    ; "go"
    ];
  [%expect
    {|
    > position startpos moves e2e4
    > position fen not a valid fen at all
    < info string bad fen: Fen: bad fullmove number "all"
    > position startpos moves e2e5
    < info string unrecognised move e2e5
    > setoption name Hash value 64
    > stop
    > kwyjibo
    >
    > go
    < bestmove a1a1
    |}]
;;

let%expect_test "ucinewgame resets" =
  For_testing.simulate [ "position startpos moves e2e4 e7e5 g1f3"; "ucinewgame"; "disp" ];
  [%expect
    {|
    > position startpos moves e2e4 e7e5 g1f3
    > ucinewgame
    > disp
    < 8 r n b q k b n r   White to move
    < 7 p p p p p p p p   castling KQkq
    < 6 . . . . . . . .   en passant -
    < 5 . . . . . . . .   halfmove 0
    < 4 . . . . . . . .   fullmove 1
    < 3 . . . . . . . .
    < 2 P P P P P P P P
    < 1 R N B Q K B N R
    <   a b c d e f g h
    |}]
;;
