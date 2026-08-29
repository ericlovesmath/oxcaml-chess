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
