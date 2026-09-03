(** Pseudolegal move generation *)

open Chess_primitives

(** Pseudolegal moves for the side to move.

    NOTE: No [zero_alloc] annotation, for the reason given on [Movelist.create]. *)
val generate : Position.t @ local -> Movelist.t @ local unique

(** Find move associated with UCI move at given position *)
val find : Position.t @ local -> string -> Move.t or_null
