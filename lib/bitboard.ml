open Core
module I = Stdlib_upstream_compatible.Int64_u
module Intrinsics = Ocaml_intrinsics_kernel.Int64.Unboxed

type t = int64#
type square = int

let empty = #0L
let full = I.lognot empty
let square ~rank ~file = (rank * 8) + file
let rank_of sq = sq / 8
let file_of sq = sq mod 8
let of_square sq = I.shift_left #1L sq
let mem t sq = I.equal (I.logand (I.shift_right_logical t sq) #1L) #1L
let set t sq = I.logor t (of_square sq)
let unset t sq = I.logand t (I.lognot (of_square sq))

(* An explicit loop rather than [List.fold_left]: there is no layout polymorphism, so an
   [int64#] cannot be a fold accumulator. It can be a function parameter. *)
let of_squares squares =
  let rec go acc = function
    | [] -> acc
    | sq :: rest -> go (I.logor acc (of_square sq)) rest
  in
  go empty squares
;;

let equal a b = I.equal a b
let is_empty t = I.equal t #0L
let count t = I.to_int (Intrinsics.count_set_bits t)
let lowest_square t = I.to_int (Intrinsics.count_trailing_zeros t)
let highest_square t = 63 - I.to_int (Intrinsics.count_leading_zeros t)

let to_string t =
  let render_line rank =
    List.init 8 ~f:(fun file -> if mem t (square ~rank ~file) then 'x' else '.')
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
  let e4 = square ~rank:3 ~file:4 in
  let d5 = square ~rank:4 ~file:3 in
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
