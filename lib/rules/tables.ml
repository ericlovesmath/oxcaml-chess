open Core
open Chess_primitives
module B = Bitboard

(** Steps outward until board ends *)
let walk square direction =
  (* NOTE: [@inline] is here to let the optimizer turn this into a loop to avoid closures *)
  let[@inline] rec go b acc =
    if B.is_empty b
    then acc
    else (
      let b = B.shift b direction in
      go b B.(acc lor b))
  in
  go (B.of_square square) B.empty
;;

let table direction =
  Int64_u.Array.init 64 ~f:(fun i -> walk (Square.unsafe_of_int i) direction)
;;

(* Top level, so built once at init - [ray]'s [zero_alloc strict] is what keeps them there *)
let north = table North
let south = table South
let east = table East
let west = table West
let north_east = table North_east
let north_west = table North_west
let south_east = table South_east
let south_west = table South_west

let ray (square : Square.t) (direction : B.Direction.t) =
  let table =
    match direction with
    | North -> north
    | South -> south
    | East -> east
    | West -> west
    | North_east -> north_east
    | North_west -> north_west
    | South_east -> south_east
    | South_west -> south_west
  in
  Int64_u.Array.unsafe_get table (square :> int)
[@@inline]
;;

(** Union of [f] over every direction *)
let all_directions ~f =
  B.Direction.all
  |> (List.map [@kind value_or_null bits64]) ~f
  |> (List.fold [@kind bits64 bits64]) ~init:B.empty ~f:B.( lor )
;;

let kings =
  Int64_u.Array.init 64 ~f:(fun i ->
    all_directions ~f:(B.shift (B.of_square (Square.unsafe_of_int i))))
;;

let king (square : Square.t) = Int64_u.Array.unsafe_get kings (square :> int) [@@inline]

let%expect_test "every ray out of a square" =
  let star square = all_directions ~f:(ray square) in
  let test square =
    square |> Square.of_string_exn |> star |> B.to_string |> print_endline
  in
  test "d4";
  test "a1";
  test "h8";
  [%expect
    {|
    8 . . . x . . . x
    7 x . . x . . x .
    6 . x . x . x . .
    5 . . x x x . . .
    4 x x x . x x x x
    3 . . x x x . . .
    2 . x . x . x . .
    1 x . . x . . x .
      a b c d e f g h

    8 x . . . . . . x
    7 x . . . . . x .
    6 x . . . . x . .
    5 x . . . x . . .
    4 x . . x . . . .
    3 x . x . . . . .
    2 x x . . . . . .
    1 . x x x x x x x
      a b c d e f g h

    8 x x x x x x x .
    7 . . . . . . x x
    6 . . . . . x . x
    5 . . . . x . . x
    4 . . . x . . . x
    3 . . x . . . . x
    2 . x . . . . . x
    1 x . . . . . . x
      a b c d e f g h
    |}]
;;

let%expect_test "king tests" =
  let test square =
    square |> Square.of_string_exn |> king |> B.to_string |> print_endline
  in
  test "d4";
  test "a1";
  test "h8";
  [%expect
    {|
    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . x x x . . .
    4 . . x . x . . .
    3 . . x x x . . .
    2 . . . . . . . .
    1 . . . . . . . .
      a b c d e f g h

    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . . . . .
    3 . . . . . . . .
    2 x x . . . . . .
    1 . x . . . . . .
      a b c d e f g h

    8 . . . . . . x .
    7 . . . . . . x x
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . . . . .
    3 . . . . . . . .
    2 . . . . . . . .
    1 . . . . . . . .
      a b c d e f g h
    |}]
;;
