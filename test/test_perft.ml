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

let rec any_legal (position @ local) moves =
  match Movegen.Movelist.pop moves with
  | #(Null, _) -> false
  | #(This move, rest) ->
    (not (Position.mover_in_check (Position.make_move position move)))
    || any_legal position rest
;;

let has_legal_move (position @ local) =
  any_legal position (Movegen.generate position (Movegen.Movelist.create ()))
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
  visit position (Movegen.generate position (Movegen.Movelist.create ())) depth counts

and visit (position @ local) moves depth counts =
  match Movegen.Movelist.pop moves with
  | #(Null, _) -> ()
  | #(This move, rest) ->
    let after = Position.make_move position move in
    if not (Position.mover_in_check after)
    then if depth = 1 then record counts move after else walk after (depth - 1) counts;
    visit position rest depth counts
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

let%expect_test "perft from startpos" =
  table Test_positions.startpos ~depths:5;
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
  table Test_positions.kiwipete ~depths:4;
  [%expect
    {|
    depth     nodes  captures      e.p.   castles    promos    checks     mates
        1        48         8         0         2         0         0         0
        2      2039       351         1        91         0         3         0
        3     97862     17102        45      3162         0       993         1
        4   4085603    757163      1929    128013     15172     25523        43
    |}]
;;

let%expect_test "perft from endgame" =
  table Test_positions.endgame ~depths:4;
  [%expect
    {|
    depth     nodes  captures      e.p.   castles    promos    checks     mates
        1        14         1         0         0         0         2         0
        2       191        14         0         0         0        10         0
        3      2812       209         2         0         0       267         0
        4     43238      3348       123         0         0      1680        17
    |}]
;;

let%expect_test "perft from promotions" =
  table Test_positions.promotions ~depths:4;
  [%expect
    {|
    depth     nodes  captures      e.p.   castles    promos    checks     mates
        1         6         0         0         0         0         0         0
        2       264        87         0         6        48        10         0
        3      9467      1021         4         0       120        38        22
        4    422333    131393         0      7795     60032     15492         5
    |}]
;;

let%expect_test "perft from tricky" =
  table Test_positions.tricky ~depths:4;
  [%expect
    {|
    depth     nodes  captures      e.p.   castles    promos    checks     mates
        1        44         6         0         1         4         0         0
        2      1486       222         0         0         0       117         0
        3     62379      8517         0      1081      5068      1201        44
        4   2103487    296153         0         0         0    158486       240
    |}]
;;

let%expect_test "perft from midgame" =
  table Test_positions.midgame ~depths:4;
  [%expect
    {|
    depth     nodes  captures      e.p.   castles    promos    checks     mates
        1        46         4         0         0         0         1         0
        2      2079       203         0         0         0        40         0
        3     89890      9470         0         0         0      1783         0
        4   3894594    440388         0         0         0     68985         0
    |}]
;;
