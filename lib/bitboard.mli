(** Unboxed bitboard, representing a set of squares on the board *)

type t = int64#

val empty : t
val full : t
val of_square : rank:int -> file:int -> t
val union : t -> t -> t
val inter : t -> t -> t
val mem : t -> rank:int -> file:int -> bool
