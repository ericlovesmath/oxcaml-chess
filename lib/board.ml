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
  B.count t.#pawns
  + B.count t.#knights
  + B.count t.#bishops
  + B.count t.#rooks
  + B.count t.#queens
  + B.count t.#kings
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

let to_string t =
  let char_at t square =
    match kind_at t square, color_at t square with
    | This kind, This color -> Piece.to_char #{ color; kind }
    | _ -> '.'
  in
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

let example =
  let place board pieces =
    let rec go board = function
      | [] -> board
      | (color, kind, square) :: rest ->
        go (toggle_piece board #{ color; kind } (Square.of_string_exn square)) rest
    in
    go board pieces
  in
  place
    empty
    [ Piece.Color.White, Piece.Kind.King, "e1"
    ; White, Rook, "a1"
    ; White, Rook, "h1"
    ; White, Knight, "f3"
    ; White, Pawn, "e4"
    ; Black, King, "e8"
    ; Black, Queen, "d8"
    ; Black, Bishop, "c5"
    ; Black, Pawn, "d5"
    ]
;;

let%expect_test "test sample board passes invariants" =
  invariant example;
  print_endline (to_string example);
  [%expect
    {|
    8 . . . q k . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . b p . . . .
    4 . . . . P . . .
    3 . . . . . N . .
    2 . . . . . . . .
    1 R . . . K . . R
      a b c d e f g h
    |}];
  let b = toggle_piece example #{ color = White; kind = King } (Square.of_string_exn "b3") in
  invariant b;
  print_endline (to_string b);
  [%expect
    {|
    8 . . . q k . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . b p . . . .
    4 . . . . P . . .
    3 . K . . . N . .
    2 . . . . . . . .
    1 R . . . K . . R
      a b c d e f g h
    |}];
  printf
    "occupied=%d white=%d black=%d kings=%s %s\n"
    (B.count (occupancy example))
    (B.count (color_board example White))
    (B.count (color_board example Black))
    (Square.to_string (king_square example White))
    (Square.to_string (king_square example Black));
  [%expect {| occupied=9 white=5 black=4 kings=e1 e8 |}]
;;

let%expect_test "a kind board covers both colors" =
  printf
    "rooks=%d kings=%d white rooks=%d black kings=%d\n"
    (B.count (kind_board example Rook))
    (B.count (kind_board example King))
    (B.count (piece_board example #{ color = White; kind = Rook }))
    (B.count (piece_board example #{ color = Black; kind = King }));
  [%expect {| rooks=2 kings=2 white rooks=2 black kings=1 |}]
;;
