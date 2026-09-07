open Core

type kind =
  | Normal
  | Double_push
  | En_passant
  | Castle
[@@deriving equal]

type t = int

(* Bit layout: from 6 | to_ 6 | moved 3 | captured 3 | promotion 3 | kind 2 *)

let to_shift = 6
let moved_shift = 12
let captured_shift = 15
let promotion_shift = 18
let kind_shift = 21
let absent = 6

let index_of_move_kind = function
  | Normal -> 0
  | Double_push -> 1
  | En_passant -> 2
  | Castle -> 3
;;

let move_kind_of_index = function
  | 0 -> Normal
  | 1 -> Double_push
  | 2 -> En_passant
  | _ -> Castle
;;

(* Field Accessors *)

let from t = Square.unsafe_of_int (t land 0x3F)
let to_ t = Square.unsafe_of_int ((t lsr to_shift) land 0x3F)
let kind t = move_kind_of_index ((t lsr kind_shift) land 0x3)

(** The three bits at [shift], holds piece kind or [absent] *)
let kind_index t shift = (t lsr shift) land 0x7

let moved t = Piece.Kind.unsafe_of_index (kind_index t moved_shift)

let kind_field t shift : Piece.Kind.t or_null =
  let index = kind_index t shift in
  if index = absent then Null else This (Piece.Kind.unsafe_of_index index)
;;

let captured t = kind_field t captured_shift
let promotion t = kind_field t promotion_shift

let captured_square t =
  match kind t with
  | En_passant -> Square.create ~rank:(Square.rank (from t)) ~file:(Square.file (to_ t))
  | Normal | Double_push | Castle -> to_ t
;;

(** Which way the king went on castle *)
let castle_side t : Castling.Side.t =
  if Square.file (to_ t) = 6 then Kingside else Queenside
;;

let castle_rook t =
  let rank = Square.rank (to_ t) in
  match castle_side t with
  | Kingside -> #(Square.create ~rank ~file:7, Square.create ~rank ~file:5)
  | Queenside -> #(Square.create ~rank ~file:0, Square.create ~rank ~file:3)
;;

(* [absent] where there is no piece *)
let index_of_piece = function
  | Null -> absent
  | This kind -> Piece.Kind.to_index kind
;;

let create ~from ~to_ ~moved ~captured ~promotion ~kind =
  (from : Square.t :> int)
  lor ((to_ : Square.t :> int) lsl to_shift)
  lor (Piece.Kind.to_index moved lsl moved_shift)
  lor (index_of_piece captured lsl captured_shift)
  lor (index_of_piece promotion lsl promotion_shift)
  lor (index_of_move_kind kind lsl kind_shift)
;;

(* Constructors *)

let quiet ~moved ~from ~to_ =
  create ~from ~to_ ~moved ~captured:Null ~promotion:Null ~kind:Normal
;;

let capture ~moved ~captured ~from ~to_ =
  create ~from ~to_ ~moved ~captured:(This captured) ~promotion:Null ~kind:Normal
;;

let double_push ~from =
  let rank = if Square.rank from = 1 then 3 else 4 in
  let to_ = Square.create ~rank ~file:(Square.file from) in
  create ~from ~to_ ~moved:Pawn ~captured:Null ~promotion:Null ~kind:Double_push
;;

let en_passant ~from ~to_ =
  create ~from ~to_ ~moved:Pawn ~captured:(This Pawn) ~promotion:Null ~kind:En_passant
;;

let castle ~(color : Piece.Color.t) ~(side : Castling.Side.t) =
  let rank =
    match color with
    | White -> 0
    | Black -> 7
  in
  let to_file =
    match side with
    | Kingside -> 6
    | Queenside -> 2
  in
  create
    ~from:(Square.create ~rank ~file:4)
    ~to_:(Square.create ~rank ~file:to_file)
    ~moved:King
    ~captured:Null
    ~promotion:Null
    ~kind:Castle
;;

let promote ~to_kind ~captured ~from ~to_ =
  create ~from ~to_ ~moved:Pawn ~captured ~promotion:(This to_kind) ~kind:Normal
;;

let is_capture t = kind_index t captured_shift <> absent
let equal (a : int) (b : int) = a = b

let to_string t =
  Square.to_string (from t)
  ^ Square.to_string (to_ t)
  ^
  match promotion t with
  | Null -> ""
  | This kind -> String.of_char (Piece.Kind.to_char kind)
;;

let file_char square = Char.of_int_exn (Char.to_int 'a' + Square.file square)

(* Standard algebraic notation *)
let san move =
  match kind move with
  | Castle ->
    (match castle_side move with
     | Kingside -> "O-O"
     | Queenside -> "O-O-O")
  | Normal | Double_push | En_passant ->
    let mover =
      match moved move with
      | Pawn -> if is_capture move then String.of_char (file_char (from move)) else ""
      | kind -> String.of_char (Char.uppercase (Piece.Kind.to_char kind))
    in
    let promotion =
      match promotion move with
      | Null -> ""
      | This kind -> sprintf "=%c" (Char.uppercase (Piece.Kind.to_char kind))
    in
    sprintf
      "%s%s%s%s"
      mover
      (if is_capture move then "x" else "")
      (Square.to_string (to_ move))
      promotion
;;

let diagram ~color moves =
  let symbol square =
    let is_to move = Square.equal (to_ move) square in
    let is_victim move =
      equal_kind (kind move) En_passant && Square.equal (captured_square move) square
    in
    match List.find moves ~f:(fun move -> Square.equal (from move) square) with
    | Some move -> Piece.to_char #{ color; kind = moved move }
    | None when List.exists moves ~f:is_to -> '*'
    | None when List.exists moves ~f:is_victim -> 'x'
    | None -> '.'
  in
  let rank_line rank =
    List.init 8 ~f:(fun file -> symbol (Square.create ~rank ~file))
    |> List.intersperse ~sep:' '
    |> String.of_list
    |> sprintf "%d %s" (rank + 1)
  in
  List.init 8 ~f:rank_line
  |> List.cons "  a b c d e f g h"
  |> List.rev
  |> String.concat_lines
;;

let sq = Square.of_string_exn

let%expect_test "every constructor tested" =
  let case name move = printf "%-16s %-8s %s\n" name (san move) (to_string move) in
  case "quiet" (quiet ~moved:Knight ~from:(sq "g1") ~to_:(sq "f3"));
  case "capture" (capture ~moved:Knight ~captured:Pawn ~from:(sq "g1") ~to_:(sq "f3"));
  case "double_push" (double_push ~from:(sq "e2"));
  case "en_passant" (en_passant ~from:(sq "e5") ~to_:(sq "d6"));
  case "castle kingside" (castle ~color:White ~side:Kingside);
  case "castle queenside" (castle ~color:White ~side:Queenside);
  case "promote" (promote ~to_kind:Queen ~captured:Null ~from:(sq "a7") ~to_:(sq "a8"));
  case
    "promote capture"
    (promote ~to_kind:Queen ~captured:(This Rook) ~from:(sq "a7") ~to_:(sq "b8"));
  [%expect
    {|
    quiet            Nf3      g1f3
    capture          Nxf3     g1f3
    double_push      e4       e2e4
    en_passant       exd6     e5d6
    castle kingside  O-O      e1g1
    castle queenside O-O-O    e1c1
    promote          a8=Q     a7a8q
    promote capture  axb8=Q   a7b8q
    |}]
;;

let%expect_test "castle derives both squares" =
  print_endline
    (diagram
       ~color:White
       [ castle ~color:White ~side:Kingside
       ; castle ~color:White ~side:Queenside
       ; castle ~color:Black ~side:Kingside
       ; castle ~color:Black ~side:Queenside
       ]);
  [%expect
    {|
    8 . . * . K . * .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . . . . . .
    3 . . . . . . . .
    2 . . . . . . . .
    1 . . * . K . * .
      a b c d e f g h
    |}]
;;

let%expect_test "double_push derives the destination" =
  print_endline
    (diagram
       ~color:White
       [ double_push ~from:(sq "a2")
       ; double_push ~from:(sq "e2")
       ; double_push ~from:(sq "h2")
       ; double_push ~from:(sq "b7")
       ; double_push ~from:(sq "g7")
       ]);
  [%expect
    {|
    8 . . . . . . . .
    7 . P . . . . P .
    6 . . . . . . . .
    5 . * . . . . * .
    4 * . . . * . . *
    3 . . . . . . . .
    2 P . . . P . . P
    1 . . . . . . . .
      a b c d e f g h
    |}]
;;

let%expect_test "en passant test" =
  print_endline (diagram ~color:White [ en_passant ~from:(sq "e5") ~to_:(sq "d6") ]);
  print_endline (diagram ~color:Black [ en_passant ~from:(sq "d4") ~to_:(sq "e3") ]);
  [%expect
    {|
    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . * . . . .
    5 . . . x P . . .
    4 . . . . . . . .
    3 . . . . . . . .
    2 . . . . . . . .
    1 . . . . . . . .
      a b c d e f g h

    8 . . . . . . . .
    7 . . . . . . . .
    6 . . . . . . . .
    5 . . . . . . . .
    4 . . . p x . . .
    3 . . . . * . . .
    2 . . . . . . . .
    1 . . . . . . . .
      a b c d e f g h
    |}]
;;
