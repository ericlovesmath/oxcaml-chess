open Core

module Side = struct
  type t =
    | Kingside
    | Queenside
  [@@deriving enumerate, equal]
end

type t = int

let bit (color : Piece.Color.t) (side : Side.t) =
  let index =
    match color, side with
    | White, Kingside -> 0
    | White, Queenside -> 1
    | Black, Kingside -> 2
    | Black, Queenside -> 3
  in
  1 lsl index
;;

let none = 0
let all = 0b1111
let mem t color side = t land bit color side <> 0
let add t color side = t lor bit color side
let remove t color side = t land lnot (bit color side)
let remove_color t color = remove (remove t color Kingside) color Queenside
let equal (a : int) (b : int) = a = b

let all_rights =
  List.concat_map Piece.Color.all ~f:(fun color ->
    List.map Side.all ~f:(fun side -> color, side))
;;

let to_char (color : Piece.Color.t) (side : Side.t) =
  let letter =
    match side with
    | Kingside -> 'k'
    | Queenside -> 'q'
  in
  match color with
  | White -> Char.uppercase letter
  | Black -> letter
;;

let to_string t =
  if t = 0
  then "-"
  else
    all_rights
    |> List.filter_map ~f:(fun (color, side) ->
      if mem t color side then Some (to_char color side) else None)
    |> String.of_list
;;

let of_string s =
  let t =
    List.fold all_rights ~init:none ~f:(fun t (color, side) ->
      if String.mem s (to_char color side) then add t color side else t)
  in
  Option.some_if (String.equal (to_string t) s) t
;;

let%expect_test "check all bits" =
  List.map all_rights ~f:(fun (color, side) ->
    [%sexp
      { right = (String.of_char (to_char color side) : string)
      ; bit = (bit color side : int)
      }])
  |> Expectable.print;
  [%expect
    {|
    ┌───────┬─────┐
    │ right │ bit │
    ├───────┼─────┤
    │ K     │ 1   │
    │ Q     │ 2   │
    │ k     │ 4   │
    │ q     │ 8   │
    └───────┴─────┘
    |}]
;;

let%expect_test "remove and remove_color" =
  print_endline (to_string all);
  print_endline (to_string (remove all White Kingside));
  print_endline (to_string (remove_color all White));
  [%expect {|
    KQkq
    Qkq
    kq
    |}]
;;

let%expect_test "round trip tests and failure tests" =
  [ "KQkq"; "Kq"; "-"; "q"; "qk"; "KQkqx"; "X"; ""; "KK" ]
  |> List.map ~f:(fun input ->
    [%sexp { input : string; valid = (Option.is_some (of_string input) : bool) }])
  |> Expectable.print;
  [%expect
    {|
    ┌───────┬───────┐
    │ input │ valid │
    ├───────┼───────┤
    │ KQkq  │ true  │
    │ Kq    │ true  │
    │ -     │ true  │
    │ q     │ true  │
    │ qk    │ false │
    │ KQkqx │ false │
    │ X     │ false │
    │       │ false │
    │ KK    │ false │
    └───────┴───────┘
    |}]
;;
