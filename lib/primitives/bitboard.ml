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

  let delta = function
    | North -> 8
    | South -> -8
    | East -> 1
    | West -> -1
    | North_east -> 9
    | North_west -> 7
    | South_east -> -7
    | South_west -> -9
  ;;
end

(** The file a shift must not wrap into *)
let edge_mask (direction : Direction.t) =
  match direction with
  | North | South -> full
  | East | North_east | South_east -> I.lognot (file_mask 0)
  | West | North_west | South_west -> I.lognot (file_mask 7)
;;

let shift t direction =
  let delta = Direction.delta direction in
  let shifted =
    if delta > 0 then I.shift_left t delta else I.shift_right_logical t (-delta)
  in
  I.logand shifted (edge_mask direction)
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
  List.map Direction.all ~f:(fun direction ->
    [%sexp
      { direction = (Direction.to_string direction : string)
      ; steps_to = (Square.to_string (lowest_square (shift d4 direction)) : string)
      }])
  |> Expectable.print;
  [%expect
    {|
    ┌────────────┬──────────┐
    │ direction  │ steps_to │
    ├────────────┼──────────┤
    │ North      │ d5       │
    │ South      │ d3       │
    │ East       │ e4       │
    │ West       │ c4       │
    │ North_east │ e5       │
    │ North_west │ c5       │
    │ South_east │ e3       │
    │ South_west │ c3       │
    └────────────┴──────────┘
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

let%expect_test "delta is the step that shift takes" =
  let checked direction =
    let agrees square =
      let stepped = shift (of_square square) direction in
      is_empty stepped
      || (lowest_square stepped :> int) = (square :> int) + Direction.delta direction
    in
    [%sexp
      { direction = (Direction.to_string direction : string)
      ; agrees = (List.for_all Square.all ~f:agrees : bool)
      }]
  in
  List.map Direction.all ~f:checked |> Expectable.print;
  [%expect
    {|
    ┌────────────┬────────┐
    │ direction  │ agrees │
    ├────────────┼────────┤
    │ North      │ true   │
    │ South      │ true   │
    │ East       │ true   │
    │ West       │ true   │
    │ North_east │ true   │
    │ North_west │ true   │
    │ South_east │ true   │
    │ South_west │ true   │
    └────────────┴────────┘
    |}]
;;
