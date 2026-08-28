(** Unboxed bitboard, representing a set of squares on the board *)

type t = int64#

val empty : t
val full : t
val of_square : rank:int -> file:int -> t [@@zero_alloc strict]
val union : t -> t -> t [@@zero_alloc strict]
val inter : t -> t -> t [@@zero_alloc strict]
val mem : t -> rank:int -> file:int -> bool [@@zero_alloc strict]
