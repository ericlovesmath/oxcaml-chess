(** Static evaluation of board in centipawns, positive for the side to move *)
val evaluate : Position.t @ local -> int
[@@zero_alloc strict]
