open Core
open Oxcaml_chess

let () = Bitboard.empty |> Bitboard.mem ~rank:0 ~file:0 |> string_of_bool |> print_endline
