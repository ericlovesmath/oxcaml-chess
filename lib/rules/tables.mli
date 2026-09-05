(** Attack tables, precomputed once at startup *)

open Chess_primitives

(** [ray square direction] is every square from [square] in [direction] to the edge of the
    board, excluding [square]. [zero_alloc strict] so tables must be built on module init. *)
val ray : Square.t -> Bitboard.Direction.t -> Bitboard.t
[@@zero_alloc strict]
