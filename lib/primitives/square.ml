open Core

type t = int

let create ~rank ~file = (rank * 8) + file
let rank t = t / 8
let file t = t mod 8
let unsafe_of_int i = i
let equal a b = a = b
let all = List.init 64 ~f:unsafe_of_int
let to_string t = sprintf "%c%d" (Char.of_int_exn (Char.to_int 'a' + file t)) (rank t + 1)
let of_string s = List.find all ~f:(fun sq -> String.equal (to_string sq) s)

let of_string_exn s =
  match of_string s with
  | Some sq -> sq
  | None -> invalid_arg (sprintf "Square.of_string_exn: %S is not a square" s)
;;

let%expect_test "testing of_string" =
  [ "a1"; "h1"; "a8"; "h8"; "e4"; "d5" ]
  |> List.map ~f:(fun square ->
    let sq = of_string_exn square in
    [%sexp
      { square : string
      ; rank = (rank sq : int)
      ; file = (file sq : int)
      ; index = ((sq :> int) : int)
      }])
  |> Expectable.print;
  [%expect
    {|
    ┌────────┬──────┬──────┬───────┐
    │ square │ rank │ file │ index │
    ├────────┼──────┼──────┼───────┤
    │ a1     │ 0    │ 0    │  0    │
    │ h1     │ 0    │ 7    │  7    │
    │ a8     │ 7    │ 0    │ 56    │
    │ h8     │ 7    │ 7    │ 63    │
    │ e4     │ 3    │ 4    │ 28    │
    │ d5     │ 4    │ 3    │ 35    │
    └────────┴──────┴──────┴───────┘
    |}];
  [ "i1"; "a9"; "a0"; "e"; ""; "e44"; "4e" ]
  |> List.map ~f:(fun input ->
    let valid = Option.is_some (of_string input) in
    [%sexp { input : string; valid : bool }])
  |> Expectable.print;
  [%expect
    {|
    ┌───────┬───────┐
    │ input │ valid │
    ├───────┼───────┤
    │ i1    │ false │
    │ a9    │ false │
    │ a0    │ false │
    │ e     │ false │
    │       │ false │
    │ e44   │ false │
    │ 4e    │ false │
    └───────┴───────┘
    |}]
;;
