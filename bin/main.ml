open Core
open Oxcaml_chess

let () = Bitboard.count Bitboard.empty |> string_of_int |> print_endline
