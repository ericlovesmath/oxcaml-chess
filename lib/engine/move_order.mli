open Chess_primitives

(** Scoring function for move order, higher scores should be evaluated first.

    Uses MVV-LVA (captures ranked by most valuable victim then least valuable aggressor). *)
val score : Move.t -> int
[@@zero_alloc strict]
