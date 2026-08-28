(** Unboxed bitboard, representing a set of squares on the board. Bit [i] is set iff
    square [i] is a member, where [i = rank * 8 + file]. *)

type t = int64#

val empty : t
val full : t
val union : t -> t -> t [@@zero_alloc strict]
val inter : t -> t -> t [@@zero_alloc strict]
val complement : t -> t [@@zero_alloc strict]
val diff : t -> t -> t [@@zero_alloc strict]
val equal : t -> t -> bool [@@zero_alloc strict]
val count : t -> int [@@zero_alloc strict]
