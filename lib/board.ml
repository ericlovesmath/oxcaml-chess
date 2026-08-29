open Core
module B = Bitboard

type t =
  #{ pawns : Bitboard.t
   ; knights : Bitboard.t
   ; bishops : Bitboard.t
   ; rooks : Bitboard.t
   ; queens : Bitboard.t
   ; kings : Bitboard.t
   ; white : Bitboard.t
   ; black : Bitboard.t
   ; occupancy : Bitboard.t
   }

let empty =
  #{ pawns = B.empty
   ; knights = B.empty
   ; bishops = B.empty
   ; rooks = B.empty
   ; queens = B.empty
   ; kings = B.empty
   ; white = B.empty
   ; black = B.empty
   ; occupancy = B.empty
   }
;;

let kind_board t (kind : Piece.Kind.t) =
  match kind with
  | Pawn -> t.#pawns
  | Knight -> t.#knights
  | Bishop -> t.#bishops
  | Rook -> t.#rooks
  | Queen -> t.#queens
  | King -> t.#kings
;;

let color_board t (color : Piece.Color.t) =
  match color with
  | White -> t.#white
  | Black -> t.#black
;;

let piece_board t (piece : Piece.t) =
  B.(kind_board t piece.#kind land color_board t piece.#color)
;;

let occupancy t = t.#occupancy

let kind_at t square : Piece.Kind.t or_null =
  if not (B.mem t.#occupancy square)
  then Null
  else if B.mem t.#pawns square
  then This Pawn
  else if B.mem t.#knights square
  then This Knight
  else if B.mem t.#bishops square
  then This Bishop
  else if B.mem t.#rooks square
  then This Rook
  else if B.mem t.#queens square
  then This Queen
  else if B.mem t.#kings square
  then This King
  else Null
;;

let color_at t square : Piece.Color.t or_null =
  if B.mem t.#white square
  then This White
  else if B.mem t.#black square
  then This Black
  else Null
;;

let king_square t color =
  B.(kind_board t King land color_board t color) |> B.lowest_square
;;

let toggle t (piece : Piece.t) mask =
  let t =
    match piece.#kind with
    | Pawn -> #{ t with pawns = B.(t.#pawns lxor mask) }
    | Knight -> #{ t with knights = B.(t.#knights lxor mask) }
    | Bishop -> #{ t with bishops = B.(t.#bishops lxor mask) }
    | Rook -> #{ t with rooks = B.(t.#rooks lxor mask) }
    | Queen -> #{ t with queens = B.(t.#queens lxor mask) }
    | King -> #{ t with kings = B.(t.#kings lxor mask) }
  in
  let occupancy = B.(t.#occupancy lxor mask) in
  match piece.#color with
  | White -> #{ t with white = B.(t.#white lxor mask); occupancy }
  | Black -> #{ t with black = B.(t.#black lxor mask); occupancy }
;;

let toggle_piece t piece square = toggle t piece (B.of_square square)
let move_piece t piece ~from ~to_ = toggle t piece B.(of_square from lor of_square to_)

let all_kinds t =
  B.(t.#pawns lor t.#knights lor t.#bishops lor t.#rooks lor t.#queens lor t.#kings)
;;

let kind_count t =
  List.sum (module Int) Piece.Kind.all ~f:(fun kind -> B.count (kind_board t kind))
;;

let invariant t =
  let check condition message =
    if not condition then failwith ("Board.invariant: " ^ message)
  in
  check (B.is_empty B.(t.#white land t.#black)) "a square is both white and black";
  check
    (B.equal t.#occupancy B.(t.#white lor t.#black))
    "occupancy is not white lor black";
  check
    (B.equal (all_kinds t) t.#occupancy)
    "the kind boards do not cover exactly the occupied squares";
  check (kind_count t = B.count t.#occupancy) "two kinds share a square"
;;

(* The FEN letter for a square, or ['.'] when it is empty *)
let char_at t square =
  match kind_at t square, color_at t square with
  | This kind, This color -> Piece.to_char #{ color; kind }
  | _ -> '.'
;;

let to_string t =
  let render_line rank =
    List.init 8 ~f:(fun file -> char_at t (Square.create ~rank ~file))
    |> List.intersperse ~sep:' '
    |> String.of_list
    |> Printf.sprintf "%d %s" (rank + 1)
  in
  List.init 8 ~f:render_line
  |> List.cons "  a b c d e f g h"
  |> List.rev
  |> String.concat_lines
;;

let start =
  let back_rank =
    [| Piece.Kind.Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook |]
  in
  let rec go board file =
    if file = 8
    then board
    else (
      let put board color kind rank =
        toggle_piece board #{ color; kind } (Square.create ~rank ~file)
      in
      let board = put board White back_rank.(file) 0 in
      let board = put board White Pawn 1 in
      let board = put board Black Pawn 6 in
      let board = put board Black back_rank.(file) 7 in
      go board (file + 1))
  in
  go empty 0
;;

let sq = Square.of_string_exn
let piece color kind : Piece.t = #{ color; kind }
let put board color kind square = toggle_piece board (piece color kind) (sq square)

let%expect_test "start renders" =
  invariant start;
  print_endline (to_string start);
  [%expect
    {|
    8 r n b q k b n r
    7 p p p p p p p p
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . . . . .
    3 . . . . . . . .
    2 P P P P P P P P
    1 R N B Q K B N R
      a b c d e f g h
    |}]
;;

let%expect_test "invariant test" =
  let board = put (put empty White Knight "e4") Black Pawn "e4" in
  (try
     invariant board;
     print_endline "accepted"
   with
   | Failure message -> print_endline message);
  [%expect {| Board.invariant: a square is both white and black |}]
;;
