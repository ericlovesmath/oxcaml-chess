open Core
module B = Bitboard
open B.Direction

let knight square =
  let b = B.of_square square in
  let hop dir dir' = B.shift (B.shift b dir) dir' in
  B.(
    hop North_east North
    lor hop North_east East
    lor hop South_east East
    lor hop South_east South
    lor hop South_west South
    lor hop South_west West
    lor hop North_west West
    lor hop North_west North)
;;

let king square =
  let b = B.of_square square in
  let step direction = B.shift b direction in
  B.(
    step North
    lor step North_east
    lor step East
    lor step South_east
    lor step South
    lor step South_west
    lor step West
    lor step North_west)
;;

let pawn (color : Piece.Color.t) pawns =
  match color with
  | White -> B.(shift pawns North_east lor shift pawns North_west)
  | Black -> B.(shift pawns South_east lor shift pawns South_west)
;;

(* TODO: Use ray boards / magic boards / PEXT instead of naive loop *)

(* Steps outward until the board ends or a piece stops the ray *)
let rec ray ~occupancy ~direction b acc =
  let b = B.shift b direction in
  if B.is_empty b
  then acc
  else (
    let acc = B.(acc lor b) in
    if B.is_empty B.(b land occupancy) then ray ~occupancy ~direction b acc else acc)
;;

let bishop ~occupancy square =
  let ray direction = ray ~occupancy ~direction (B.of_square square) B.empty in
  B.(ray North_east lor ray South_east lor ray South_west lor ray North_west)
;;

let rook ~occupancy square =
  let ray direction = ray ~occupancy ~direction (B.of_square square) B.empty in
  B.(ray North lor ray East lor ray South lor ray West)
;;

let queen ~occupancy square = B.(bishop ~occupancy square lor rook ~occupancy square)

(** - [piece] where the attackers are
    - [*] on a square they attack
    - [x] on capturables
    - [#] on occupied squares they cannot reach *)
let show ~piece ~attackers ~occupancy attacked =
  let render_line rank =
    List.init 8 ~f:(fun file ->
      let square = Square.create ~rank ~file in
      if B.mem attackers square
      then piece
      else (
        match B.mem attacked square, B.mem occupancy square with
        | true, true -> 'x'
        | true, false -> '*'
        | false, true -> '#'
        | false, false -> '.'))
    |> List.intersperse ~sep:' '
    |> String.of_list
    |> sprintf "%d %s" (rank + 1)
  in
  List.init 8 ~f:render_line
  |> List.cons "  a b c d e f g h"
  |> List.rev
  |> String.concat_lines
  |> print_endline
;;

let%expect_test "knight moves" =
  let leaper piece attacks name =
    let square = Square.of_string_exn name in
    show ~piece ~attackers:(B.of_square square) ~occupancy:B.empty (attacks square)
  in
  List.iter [ "a1"; "b1"; "d4" ] ~f:(leaper 'N' knight);
  List.iter [ "a1"; "d4" ] ~f:(leaper 'K' king);
  [%expect
    {|
    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . . . . .
    3 . * . . . . . .
    2 . . * . . . . .
    1 N . . . . . . .
      a b c d e f g h

    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . . . . .
    3 * . * . . . . .
    2 . . . * . . . .
    1 . N . . . . . .
      a b c d e f g h

    8 . . . . . . . .
    7 . . . . . . . .
    6 . . * . * . . .
    5 . * . . . * . .
    4 . . . N . . . .
    3 . * . . . * . .
    2 . . * . * . . .
    1 . . . . . . . .
      a b c d e f g h

    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . . . . .
    3 . . . . . . . .
    2 * * . . . . . .
    1 K * . . . . . .
      a b c d e f g h

    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . * * * . . .
    4 . . * K * . . .
    3 . . * * * . . .
    2 . . . . . . . .
    1 . . . . . . . .
      a b c d e f g h
    |}]
;;

let%expect_test "pawns" =
  let rank_of rank =
    B.of_squares
      (List.map [ "a"; "d"; "h" ] ~f:(fun file -> Square.of_string_exn (file ^ rank)))
  in
  let white = rank_of "2" in
  let black = rank_of "7" in
  show ~piece:'P' ~attackers:white ~occupancy:B.empty (pawn White white);
  show ~piece:'p' ~attackers:black ~occupancy:B.empty (pawn Black black);
  [%expect
    {|
    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . . . . .
    3 . * * . * . * .
    2 P . . P . . . P
    1 . . . . . . . .
      a b c d e f g h

    8 . . . . . . . .
    7 p . . p . . . p
    6 . * * . * . * .
    5 . . . . . . . .
    4 . . . . . . . .
    3 . . . . . . . .
    2 . . . . . . . .
    1 . . . . . . . .
      a b c d e f g h
    |}]
;;

let%expect_test "ray pieces" =
  let slider piece attacks name blockers =
    let square = Square.of_string_exn name in
    let occupancy =
      B.(of_squares (List.map ~f:Square.of_string_exn blockers) lor of_square square)
    in
    show ~piece ~attackers:(B.of_square square) ~occupancy (attacks ~occupancy square)
  in
  slider 'R' rook "d4" [ "d6"; "b4"; "f7" ];
  slider 'B' bishop "d4" [ "f6"; "b6"; "g4" ];
  slider 'Q' queen "d4" [ "d6"; "b4"; "f6"; "b6" ];
  slider 'R' rook "a1" [];
  [%expect
    {|
    8 . . . . . . . .
    7 . . . . . # . .
    6 . . . x . . . .
    5 . . . * . . . .
    4 . x * R * * * *
    3 . . . * . . . .
    2 . . . * . . . .
    1 . . . * . . . .
      a b c d e f g h

    8 . . . . . . . .
    7 . . . . . . . .
    6 . x . . . x . .
    5 . . * . * . . .
    4 . . . B . . # .
    3 . . * . * . . .
    2 . * . . . * . .
    1 * . . . . . * .
      a b c d e f g h

    8 . . . . . . . .
    7 . . . . . . . .
    6 . x . x . x . .
    5 . . * * * . . .
    4 . x * Q * * * *
    3 . . * * * . . .
    2 . * . * . * . .
    1 * . . * . . * .
      a b c d e f g h

    8 * . . . . . . .
    7 * . . . . . . .
    6 * . . . . . . .
    5 * . . . . . . .
    4 * . . . . . . .
    3 * . . . . . . .
    2 * . . . . . . .
    1 R * * * * * * *
      a b c d e f g h
    |}]
;;
