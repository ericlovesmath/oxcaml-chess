open Core
open Chess_primitives
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

let king square = Tables.king square

let pawn (color : Piece.Color.t) pawns =
  match color with
  | White -> B.(shift pawns North_east lor shift pawns North_west)
  | Black -> B.(shift pawns South_east lor shift pawns South_west)
;;

(** Ray attacks (ray to the edge minus the ray behind the first blocker) *)
let[@inline] slide ~occupancy ~direction square =
  let ray = Tables.ray square direction in
  let blockers = B.(ray land occupancy) in
  if B.is_empty blockers
  then ray
  else (
    let first =
      if B.Direction.delta direction > 0
      then B.lowest_square blockers
      else B.highest_square blockers
    in
    B.(ray - Tables.ray first direction))
;;

let bishop ~occupancy square =
  let[@inline] slide direction = slide ~occupancy ~direction square in
  B.(slide North_east lor slide South_east lor slide South_west lor slide North_west)
;;

let rook ~occupancy square =
  let[@inline] slide direction = slide ~occupancy ~direction square in
  B.(slide North lor slide East lor slide South lor slide West)
;;

let queen ~occupancy square = B.(bishop ~occupancy square lor rook ~occupancy square)

(* NOTE: Shortcircuiting the attack set computations actually doesn't help that much
   because of (what I assume to be) branch prediction failures, since the filter is almost
   always just showing unattacked positions. Measured no performance loss, not worth. *)

let attackers_to board square ~by =
  let occupancy = Board.occupancy board in
  let kind_board kind = Board.kind_board board kind in
  let diagonal = B.(kind_board Bishop lor kind_board Queen) in
  let straight = B.(kind_board Rook lor kind_board Queen) in
  let attackers =
    B.(
      (* NOTE: the pawn direction is inverted on purpose. The White pawns attacking
         [square] are the ones a Black pawn standing on [square] would attack. *)
      pawn (Piece.Color.flip by) (of_square square)
      land kind_board Pawn
      lor (king square land kind_board King)
      lor (knight square land kind_board Knight)
      lor (bishop ~occupancy square land diagonal)
      lor (rook ~occupancy square land straight))
  in
  B.(attackers land Board.color_board board by)
;;

let is_attacked board square ~by = not (B.is_empty (attackers_to board square ~by))

let in_check board color =
  is_attacked board (Board.king_square board color) ~by:(Piece.Color.flip color)
;;

(* For testing *)
let board_of pieces =
  let add board piece =
    let letter = piece.[0] in
    let color = if Char.is_uppercase letter then Piece.Color.White else Black in
    let kind = Option.value_exn (Piece.Kind.of_char letter) in
    Board.toggle_piece
      board
      #{ color; kind }
      (Square.of_string_exn (String.drop_prefix piece 1))
  in
  let rec go board = function
    | [] -> board
    | piece :: rest -> go (add board piece) rest
  in
  go Board.empty pieces
;;

let show pieces color =
  let board = board_of pieces in
  let attacked =
    (List.fold [@kind value_or_null bits64])
      Square.all
      ~init:B.empty
      ~f:(fun acc square ->
        if is_attacked board square ~by:color then B.set acc square else acc)
  in
  List.iter2_exn
    (String.split_lines (Board.to_string board))
    (String.split_lines (B.to_string attacked))
    ~f:(printf "  %-17s   %s\n");
  printf "\n"
;;

let%expect_test "knight moves" =
  List.iter [ "Na1"; "Nb1"; "Nd4" ] ~f:(fun knight -> show [ knight ] White);
  [%expect
    {|
    8 . . . . . . . .   8 . . . . . . . .
    7 . . . . . . . .   7 . . . . . . . .
    6 . . . . . . . .   6 . . . . . . . .
    5 . . . . . . . .   5 . . . . . . . .
    4 . . . . . . . .   4 . . . . . . . .
    3 . . . . . . . .   3 . x . . . . . .
    2 . . . . . . . .   2 . . x . . . . .
    1 N . . . . . . .   1 . . . . . . . .
      a b c d e f g h     a b c d e f g h

    8 . . . . . . . .   8 . . . . . . . .
    7 . . . . . . . .   7 . . . . . . . .
    6 . . . . . . . .   6 . . . . . . . .
    5 . . . . . . . .   5 . . . . . . . .
    4 . . . . . . . .   4 . . . . . . . .
    3 . . . . . . . .   3 x . x . . . . .
    2 . . . . . . . .   2 . . . x . . . .
    1 . N . . . . . .   1 . . . . . . . .
      a b c d e f g h     a b c d e f g h

    8 . . . . . . . .   8 . . . . . . . .
    7 . . . . . . . .   7 . . . . . . . .
    6 . . . . . . . .   6 . . x . x . . .
    5 . . . . . . . .   5 . x . . . x . .
    4 . . . N . . . .   4 . . . . . . . .
    3 . . . . . . . .   3 . x . . . x . .
    2 . . . . . . . .   2 . . x . x . . .
    1 . . . . . . . .   1 . . . . . . . .
      a b c d e f g h     a b c d e f g h
    |}]
;;

let%expect_test "king moves" =
  List.iter [ "Ka1"; "Kd4" ] ~f:(fun king -> show [ king ] White);
  [%expect
    {|
    8 . . . . . . . .   8 . . . . . . . .
    7 . . . . . . . .   7 . . . . . . . .
    6 . . . . . . . .   6 . . . . . . . .
    5 . . . . . . . .   5 . . . . . . . .
    4 . . . . . . . .   4 . . . . . . . .
    3 . . . . . . . .   3 . . . . . . . .
    2 . . . . . . . .   2 x x . . . . . .
    1 K . . . . . . .   1 . x . . . . . .
      a b c d e f g h     a b c d e f g h

    8 . . . . . . . .   8 . . . . . . . .
    7 . . . . . . . .   7 . . . . . . . .
    6 . . . . . . . .   6 . . . . . . . .
    5 . . . . . . . .   5 . . x x x . . .
    4 . . . K . . . .   4 . . x . x . . .
    3 . . . . . . . .   3 . . x x x . . .
    2 . . . . . . . .   2 . . . . . . . .
    1 . . . . . . . .   1 . . . . . . . .
      a b c d e f g h     a b c d e f g h
    |}]
;;

let%expect_test "pawns" =
  let pawns = [ "Pa2"; "Pd2"; "Ph2"; "pa7"; "pd7"; "ph7" ] in
  show pawns White;
  show pawns Black;
  [%expect
    {|
    8 . . . . . . . .   8 . . . . . . . .
    7 p . . p . . . p   7 . . . . . . . .
    6 . . . . . . . .   6 . . . . . . . .
    5 . . . . . . . .   5 . . . . . . . .
    4 . . . . . . . .   4 . . . . . . . .
    3 . . . . . . . .   3 . x x . x . x .
    2 P . . P . . . P   2 . . . . . . . .
    1 . . . . . . . .   1 . . . . . . . .
      a b c d e f g h     a b c d e f g h

    8 . . . . . . . .   8 . . . . . . . .
    7 p . . p . . . p   7 . . . . . . . .
    6 . . . . . . . .   6 . x x . x . x .
    5 . . . . . . . .   5 . . . . . . . .
    4 . . . . . . . .   4 . . . . . . . .
    3 . . . . . . . .   3 . . . . . . . .
    2 P . . P . . . P   2 . . . . . . . .
    1 . . . . . . . .   1 . . . . . . . .
      a b c d e f g h     a b c d e f g h
    |}]
;;

let%expect_test "ray pieces" =
  show [ "Rd4"; "pd6"; "pb4"; "pf7" ] White;
  show [ "Bd4"; "pf6"; "pb6"; "pg4" ] White;
  show [ "Qd4"; "pd6"; "pb4"; "pf6"; "pb6" ] White;
  show [ "Ra1" ] White;
  [%expect
    {|
    8 . . . . . . . .   8 . . . . . . . .
    7 . . . . . p . .   7 . . . . . . . .
    6 . . . p . . . .   6 . . . x . . . .
    5 . . . . . . . .   5 . . . x . . . .
    4 . p . R . . . .   4 . x x . x x x x
    3 . . . . . . . .   3 . . . x . . . .
    2 . . . . . . . .   2 . . . x . . . .
    1 . . . . . . . .   1 . . . x . . . .
      a b c d e f g h     a b c d e f g h

    8 . . . . . . . .   8 . . . . . . . .
    7 . . . . . . . .   7 . . . . . . . .
    6 . p . . . p . .   6 . x . . . x . .
    5 . . . . . . . .   5 . . x . x . . .
    4 . . . B . . p .   4 . . . . . . . .
    3 . . . . . . . .   3 . . x . x . . .
    2 . . . . . . . .   2 . x . . . x . .
    1 . . . . . . . .   1 x . . . . . x .
      a b c d e f g h     a b c d e f g h

    8 . . . . . . . .   8 . . . . . . . .
    7 . . . . . . . .   7 . . . . . . . .
    6 . p . p . p . .   6 . x . x . x . .
    5 . . . . . . . .   5 . . x x x . . .
    4 . p . Q . . . .   4 . x x . x x x x
    3 . . . . . . . .   3 . . x x x . . .
    2 . . . . . . . .   2 . x . x . x . .
    1 . . . . . . . .   1 x . . x . . x .
      a b c d e f g h     a b c d e f g h

    8 . . . . . . . .   8 x . . . . . . .
    7 . . . . . . . .   7 x . . . . . . .
    6 . . . . . . . .   6 x . . . . . . .
    5 . . . . . . . .   5 x . . . . . . .
    4 . . . . . . . .   4 x . . . . . . .
    3 . . . . . . . .   3 x . . . . . . .
    2 . . . . . . . .   2 x . . . . . . .
    1 R . . . . . . .   1 . x x x x x x x
      a b c d e f g h     a b c d e f g h
    |}]
;;

let%expect_test "Random pieces" =
  show [ "Kb1"; "Pd2"; "Ne4"; "Bg1"; "Ra8"; "Qh5" ] White;
  [%expect
    {|
    8 R . . . . . . .   8 . x x x x x x x
    7 . . . . . . . .   7 x . . . . x . x
    6 . . . . . . . .   6 x x . x . x x x
    5 . . . . . . . Q   5 x x x x x x x .
    4 . . . . N . . .   4 x . . x . . x x
    3 . . . . . . . .   3 x . x . x x x x
    2 . . . P . . . .   2 x x x x x x . x
    1 . K . . . . B .   1 x . x x . . . x
      a b c d e f g h     a b c d e f g h
    |}]
;;
