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
  Test_positions.
    [ "startpos", startpos, 4
    ; "kiwipete", kiwipete, 4
    ; "midgame", midgame, 4
    ; "endgame", endgame, 5
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
