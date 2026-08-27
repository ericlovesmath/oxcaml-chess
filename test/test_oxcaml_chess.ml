open Oxcaml_chess

let () =
  let e4 = Bitboard.of_square ~rank:3 ~file:4 in
  assert (Bitboard.mem e4 ~rank:3 ~file:4);
  assert (not (Bitboard.mem e4 ~rank:3 ~file:5))
;;
