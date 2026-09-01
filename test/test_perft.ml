open Core
open Oxcaml_chess

(* Perft counts the number of legal moves at a given state after a certain depth.
   [https://chessprogramming.org/Perft_Results] provides us a table of known counts for
   legal moves, and we will use it as a reference to validate that our pseudo legal move
   generation and attack filtering works correctly. *)

let position_of fen =
  match Fen.to_position fen with
  | Ok position -> position
  | Error message -> failwith message
;;

type counts =
  { mutable nodes : int
  ; mutable captures : int
  ; mutable en_passant : int
  ; mutable castles : int
  ; mutable promotions : int
  ; mutable checks : int
  ; mutable checkmates : int
  }

let init () =
  { nodes = 0
  ; captures = 0
  ; en_passant = 0
  ; castles = 0
  ; promotions = 0
  ; checks = 0
  ; checkmates = 0
  }
;;

let has_legal_move (position @ local) =
  let moves = Movegen.generate position (Movegen.Movelist.create ()) in
  let n = Movegen.Movelist.length moves in
  (* TODO: Is there some way to use a [let rec local_] with a closure efficiently? *)
  let mutable i = 0 in
  let mutable found = false in
  while (not found) && i < n do
    let move = Movegen.Movelist.get moves i in
    let after = Position.make_move position move in
    if not (Position.mover_in_check after) then found <- true;
    i <- i + 1
  done;
  found
;;

let record counts move (after @ local) =
  counts.nodes <- counts.nodes + 1;
  if Move.is_capture move then counts.captures <- counts.captures + 1;
  (match Move.kind move with
   | En_passant -> counts.en_passant <- counts.en_passant + 1
   | Castle -> counts.castles <- counts.castles + 1
   | Normal | Double_push -> ());
  (match Move.promotion move with
   | Null -> ()
   | This _ -> counts.promotions <- counts.promotions + 1);
  if Position.in_check after
  then (
    counts.checks <- counts.checks + 1;
    if not (has_legal_move after) then counts.checkmates <- counts.checkmates + 1)
;;

(* A move is legal exactly when it does not leave its own king attacked *)
let rec walk (position @ local) depth counts =
  let moves = Movegen.generate position (Movegen.Movelist.create ()) in
  for i = 0 to Movegen.Movelist.length moves - 1 do
    let move = Movegen.Movelist.get moves i in
    let after = Position.make_move position move in
    if not (Position.mover_in_check after)
    then if depth = 1 then record counts move after else walk after (depth - 1) counts
  done
;;

let cell width text = String.pad_left text ~len:width
let row cells = List.map cells ~f:(cell 10) |> String.concat

let table fen ~depths =
  let position = position_of fen in
  row [ "depth"; "nodes"; "captures"; "e.p."; "castles"; "promos"; "checks"; "mates" ]
  :: List.init depths ~f:(fun i ->
    let depth = i + 1 in
    let counts = init () in
    walk position depth counts;
    row
      [ Int.to_string depth
      ; Int.to_string counts.nodes
      ; Int.to_string counts.captures
      ; Int.to_string counts.en_passant
      ; Int.to_string counts.castles
      ; Int.to_string counts.promotions
      ; Int.to_string counts.checks
      ; Int.to_string counts.checkmates
      ])
  |> String.concat_lines
  |> print_string
;;

let%expect_test "perft from initial" =
  table "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" ~depths:5;
  [%expect
    {|
    depth     nodes  captures      e.p.   castles    promos    checks     mates
        1        20         0         0         0         0         0         0
        2       400         0         0         0         0         0         0
        3      8902        34         0         0         0        12         0
        4    197281      1576         0         0         0       469         8
        5   4865609     82719       258         0         0     27351       347
    |}]
;;

let%expect_test "perft from kiwipete" =
  table "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1" ~depths:4;
  [%expect
    {|
    depth     nodes  captures      e.p.   castles    promos    checks     mates
        1        48         8         0         2         0         0         0
        2      2039       351         1        91         0         3         0
        3     97862     17102        45      3162         0       993         1
        4   4085603    757163      1929    128013     15172     25523        43
    |}]
;;

let%expect_test "perft from position 3" =
  table "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1" ~depths:4;
  [%expect
    {|
    depth     nodes  captures      e.p.   castles    promos    checks     mates
        1        14         1         0         0         0         2         0
        2       191        14         0         0         0        10         0
        3      2812       209         2         0         0       267         0
        4     43238      3348       123         0         0      1680        17
    |}]
;;

let%expect_test "perft from position 4" =
  table "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1" ~depths:4;
  [%expect
    {|
    depth     nodes  captures      e.p.   castles    promos    checks     mates
        1         6         0         0         0         0         0         0
        2       264        87         0         6        48        10         0
        3      9467      1021         4         0       120        38        22
        4    422333    131393         0      7795     60032     15492         5
    |}]
;;

let%expect_test "perft from position 5" =
  table "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8" ~depths:4;
  [%expect
    {|
    depth     nodes  captures      e.p.   castles    promos    checks     mates
        1        44         6         0         1         4         0         0
        2      1486       222         0         0         0       117         0
        3     62379      8517         0      1081      5068      1201        44
        4   2103487    296153         0         0         0    158486       240
    |}]
;;

let%expect_test "perft from position 6" =
  table
    "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
    ~depths:4;
  [%expect
    {|
    depth     nodes  captures      e.p.   castles    promos    checks     mates
        1        46         4         0         0         0         1         0
        2      2079       203         0         0         0        40         0
        3     89890      9470         0         0         0      1783         0
        4   3894594    440388         0         0         0     68985         0
    |}]
;;
