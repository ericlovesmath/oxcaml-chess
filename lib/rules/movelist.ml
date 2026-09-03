open Core
open Chess_primitives

(* TODO: Check if we can shrink this, and if it matters that much *)

(** The theoretical maximum number of moves is 218, but we just pad *)
let capacity = 255

type t =
  { moves : Move.t array
  ; mutable length : int
  }

type builder = t

let dummy_move =
  let a1 = Square.unsafe_of_int 0 in
  Move.quiet ~moved:Pawn ~from:a1 ~to_:a1
;;

let push (t : builder @ local) move =
  Array.set t.moves t.length move;
  t.length <- t.length + 1
;;

let build f : t @ local unique = exclave_
  let builder = { moves = Array.create_local ~len:capacity dummy_move; length = 0 } in
  f (borrow_ builder);
  builder
;;

let length (t : t @ local) = t.length

let get (t : t @ local) i =
  if i >= t.length then invalid_arg "Movelist.get: past the generated moves";
  t.moves.(i)
;;

let find (t : t @ local) ~f =
  let mutable i = 0 in
  let mutable found = Null in
  while i < t.length do
    let move = t.moves.(i) in
    if f move
    then (
      found <- This move;
      i <- t.length)
    else i <- i + 1
  done;
  found
;;

let exists (t : t @ local) ~f =
  match find t ~f with
  | Null -> false
  | This _ -> true
;;

(* Insertion sort, descending, near-linear lol *)
let sorted (t : t @ local unique) ~score : t @ local =
  let arr = t.moves in
  for i = 1 to t.length - 1 do
    let move = arr.(i) in
    let move_score = score move in
    let mutable j = i in
    while j > 0 && score arr.(j - 1) < move_score do
      arr.(j) <- arr.(j - 1);
      j <- j - 1
    done;
    arr.(j) <- move
  done;
  t
;;
