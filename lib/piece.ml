open Core

module Color = struct
  type t =
    | White
    | Black
  [@@deriving enumerate, equal]

  let flip = function
    | White -> Black
    | Black -> White
  ;;

  let to_string = function
    | White -> "w"
    | Black -> "b"
  ;;

  let of_string = function
    | "w" -> White
    | "b" -> Black
    | s -> failwith ("Piece.Color: invalid color " ^ s)
  ;;
end

module Kind = struct
  type t =
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King
  [@@deriving enumerate, equal]

  let to_char = function
    | Pawn -> 'p'
    | Knight -> 'n'
    | Bishop -> 'b'
    | Rook -> 'r'
    | Queen -> 'q'
    | King -> 'k'
  ;;

  let to_index = function
    | Pawn -> 0
    | Knight -> 1
    | Bishop -> 2
    | Rook -> 3
    | Queen -> 4
    | King -> 5
  ;;

  let unsafe_of_index = function
    | 0 -> Pawn
    | 1 -> Knight
    | 2 -> Bishop
    | 3 -> Rook
    | 4 -> Queen
    | _ -> King
  ;;

  let of_char c =
    let c = Char.lowercase c in
    List.find all ~f:(fun kind -> Char.equal (to_char kind) c)
  ;;
end

type t =
  #{ color : Color.t
   ; kind : Kind.t
   }

let to_char t =
  let letter = Kind.to_char t.#kind in
  match t.#color with
  | White -> Char.uppercase letter
  | Black -> letter
;;

let%expect_test "FEN letters for all twelve pieces" =
  let letters =
    let open List.Let_syntax in
    let%bind color = Color.all in
    let%bind kind = Kind.all in
    return (to_char #{ color; kind })
  in
  print_endline (String.of_list letters);
  [%expect {| PNBRQKpnbrqk |}]
;;
