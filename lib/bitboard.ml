open Core
module I = Stdlib_upstream_compatible.Int64_u
module U = Ocaml_intrinsics_kernel.Int64.Unboxed

type t = int64#

let empty = #0L
let full = I.lognot empty
let of_square (sq : Square.t) = I.shift_left #1L (sq :> int)

let mem t (sq : Square.t) =
  I.equal (I.logand (I.shift_right_logical t (sq :> int)) #1L) #1L
;;

let set t sq = I.logor t (of_square sq)
let unset t sq = I.logand t (I.lognot (of_square sq))

let of_squares squares =
  let rec go acc = function
    | [] -> acc
    | sq :: rest -> go (set acc sq) rest
  in
  go empty squares
;;

let equal a b = I.equal a b
let is_empty t = I.equal t #0L
let count t = I.to_int (U.count_set_bits t)
let lowest_square t = Square.unsafe_of_int (I.to_int (U.count_trailing_zeros t))
let highest_square t = Square.unsafe_of_int (63 - I.to_int (U.count_leading_zeros t))
let rank_mask rank = I.shift_left #0xFFL (rank * 8)
let remove_lowest t = I.logand t (I.sub t #1L)
let file_mask file = I.shift_left #0x0101010101010101L file

module Direction = struct
  type t =
    | North
    | South
    | East
    | West
    | North_east
    | North_west
    | South_east
    | South_west
  [@@deriving enumerate, to_string]
end

let shift t (direction : Direction.t) =
  let non_file_a = I.lognot (file_mask 0) in
  let non_file_h = I.lognot (file_mask 7) in
  match direction with
  | North -> I.shift_left t 8
  | South -> I.shift_right_logical t 8
  | East -> I.logand (I.shift_left t 1) non_file_a
  | West -> I.logand (I.shift_right_logical t 1) non_file_h
  | North_east -> I.logand (I.shift_left t 9) non_file_a
  | North_west -> I.logand (I.shift_left t 7) non_file_h
  | South_east -> I.logand (I.shift_right_logical t 7) non_file_a
  | South_west -> I.logand (I.shift_right_logical t 9) non_file_h
;;

let to_string t =
  let render_line rank =
    List.init 8 ~f:(fun file -> if mem t (Square.create ~rank ~file) then 'x' else '.')
    |> List.intersperse ~sep:' '
    |> String.of_list
    |> Printf.sprintf "%d %s" (rank + 1)
  in
  List.init 8 ~f:render_line
  |> List.cons "  a b c d e f g h"
  |> List.rev
  |> String.concat_lines
;;

let ( land ) a b = I.logand a b
let ( lor ) a b = I.logor a b
let ( lxor ) a b = I.logxor a b
let ( - ) a b = I.logand a (I.lognot b)
let lnot t = I.lognot t

let%expect_test "to_string of the empty and full board" =
  print_endline (to_string empty);
  [%expect
    {|
    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . . . . .
    3 . . . . . . . .
    2 . . . . . . . .
    1 . . . . . . . .
      a b c d e f g h
    |}];
  print_endline (to_string full);
  [%expect
    {|
    8 x x x x x x x x
    7 x x x x x x x x
    6 x x x x x x x x
    5 x x x x x x x x
    4 x x x x x x x x
    3 x x x x x x x x
    2 x x x x x x x x
    1 x x x x x x x x
      a b c d e f g h
    |}]
;;

let%expect_test "add, remove and mem" =
  let e4 = Square.of_string_exn "e4" in
  let d5 = Square.of_string_exn "d5" in
  let board = set (set empty e4) d5 in
  print_endline (to_string board);
  [%expect
    {|
    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . x . . . .
    4 . . . . x . . .
    3 . . . . . . . .
    2 . . . . . . . .
    1 . . . . . . . .
      a b c d e f g h
    |}];
  print_endline (to_string (unset board e4));
  [%expect
    {|
    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . x . . . .
    4 . . . . . . . .
    3 . . . . . . . .
    2 . . . . . . . .
    1 . . . . . . . .
      a b c d e f g h
    |}]
;;

let%expect_test "file_mask check" =
  print_endline (to_string (file_mask 0 lor file_mask 7));
  [%expect
    {|
    8 x . . . . . . x
    7 x . . . . . . x
    6 x . . . . . . x
    5 x . . . . . . x
    4 x . . . . . . x
    3 x . . . . . . x
    2 x . . . . . . x
    1 x . . . . . . x
      a b c d e f g h
    |}]
;;

let%expect_test "each direction steps exactly one square" =
  let d4 = of_square (Square.of_string_exn "d4") in
  List.iter Direction.all ~f:(fun direction ->
    printf
      "shift d4 %-10s = %s\n"
      (Direction.to_string direction)
      (Square.to_string (lowest_square (shift d4 direction))));
  [%expect
    {|
    shift d4 North      = d5
    shift d4 South      = d3
    shift d4 East       = e4
    shift d4 West       = c4
    shift d4 North_east = e5
    shift d4 North_west = c5
    shift d4 South_east = e3
    shift d4 South_west = c3
    |}]
;;

(* The corners are the members that must vanish rather than reappear on the far side. *)
let%expect_test "stepping off an edge drops members instead of wrapping" =
  let probe =
    of_squares (List.map ~f:Square.of_string_exn [ "a1"; "h1"; "a8"; "h8"; "d4" ])
  in
  print_endline (to_string probe);
  List.iter Direction.all ~f:(fun dir ->
    [ Direction.to_string dir; to_string (shift probe dir) ]
    |> String.concat ~sep:"\n"
    |> print_endline);
  [%expect
    {|
    8 x . . . . . . x
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . x . . . .
    3 . . . . . . . .
    2 . . . . . . . .
    1 x . . . . . . x
      a b c d e f g h

    North
    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . x . . . .
    4 . . . . . . . .
    3 . . . . . . . .
    2 x . . . . . . x
    1 . . . . . . . .
      a b c d e f g h

    South
    8 . . . . . . . .
    7 x . . . . . . x
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . . . . .
    3 . . . x . . . .
    2 . . . . . . . .
    1 . . . . . . . .
      a b c d e f g h

    East
    8 . x . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . x . . .
    3 . . . . . . . .
    2 . . . . . . . .
    1 . x . . . . . .
      a b c d e f g h

    West
    8 . . . . . . x .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . x . . . . .
    3 . . . . . . . .
    2 . . . . . . . .
    1 . . . . . . x .
      a b c d e f g h

    North_east
    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . . x . . .
    4 . . . . . . . .
    3 . . . . . . . .
    2 . x . . . . . .
    1 . . . . . . . .
      a b c d e f g h

    North_west
    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . x . . . . .
    4 . . . . . . . .
    3 . . . . . . . .
    2 . . . . . . x .
    1 . . . . . . . .
      a b c d e f g h

    South_east
    8 . . . . . . . .
    7 . x . . . . . .
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . . . . .
    3 . . . . x . . .
    2 . . . . . . . .
    1 . . . . . . . .
      a b c d e f g h

    South_west
    8 . . . . . . . .
    7 . . . . . . x .
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . . . . .
    3 . . x . . . . .
    2 . . . . . . . .
    1 . . . . . . . .
      a b c d e f g h
    |}]
;;
