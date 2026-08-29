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
  List.iter [ "a1"; "h1"; "a8"; "h8"; "e4"; "d5" ] ~f:(fun s ->
    let sq = of_string_exn s in
    printf "%s = rank %d, file %d, square %d\n" s (rank sq) (file sq) (sq :> int));
  [%expect
    {|
    a1 = rank 0, file 0, square 0
    h1 = rank 0, file 7, square 7
    a8 = rank 7, file 0, square 56
    h8 = rank 7, file 7, square 63
    e4 = rank 3, file 4, square 28
    d5 = rank 4, file 3, square 35
    |}];
  List.iter [ "i1"; "a9"; "a0"; "e"; ""; "e44"; "4e" ] ~f:(fun s ->
    printf "%s is %s\n" s (if Option.is_none (of_string s) then "invalid" else "Some"));
  [%expect
    {|
    i1 is invalid
    a9 is invalid
    a0 is invalid
    e is invalid
     is invalid
    e44 is invalid
    4e is invalid
    |}]
;;
