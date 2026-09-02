open Core
open Oxcaml_chess

(* Tests for node efficiency and GC allocations *)

let san move =
  match move with
  | Null -> "none"
  | This move -> Move.san move
;;

(* [Gc.allocated_words] is [major - promoted + minor] *)
let measure (name, fen, depth) =
  let position = Fen.to_position_exn fen in
  let before = Gc.allocated_words () in
  let #{ Search.score; move; nodes } = Search.search position ~depth in
  let words = Gc.allocated_words () - before in
  [%sexp
    { position = (name : string)
    ; depth : int
    ; nodes : int
    ; words : int
    ; words_per_node = (words / nodes : int)
    ; score : int
    ; move = (san move : string)
    }]
;;

let%expect_test "search work per position" =
  [ "startpos", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", 4
  ; "kiwipete", "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 4
  ; ( "midgame"
    , "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
    , 4 )
  ; "endgame", "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 5
  ]
  |> List.map ~f:measure
  |> Expectable.print;
  [%expect
    {|
    ┌──────────┬───────┬────────┬─────────┬────────────────┬───────┬──────┐
    │ position │ depth │ nodes  │ words   │ words_per_node │ score │ move │
    ├──────────┼───────┼────────┼─────────┼────────────────┼───────┼──────┤
    │ startpos │ 4     │  28786 │  758016 │ 26             │   0   │ Nc3  │
    │ kiwipete │ 4     │ 103576 │ 3002624 │ 28             │  70   │ Bxa6 │
    │ midgame  │ 4     │ 155621 │ 3281152 │ 21             │ -90   │ Nd5  │
    │ endgame  │ 5     │  54338 │ 1706752 │ 31             │ 110   │ Rxf4 │
    └──────────┴───────┴────────┴─────────┴────────────────┴───────┴──────┘
    |}]
;;
