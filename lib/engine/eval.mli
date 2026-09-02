(** Static evaluation of board in centipawns, positive for the side to move *)

open Chess_primitives
open Chess_rules

val evaluate : Position.t @ local -> int [@@zero_alloc strict]

(** Material values in centipawns *)
val piece_value : Piece.Kind.t -> int
[@@zero_alloc strict]
