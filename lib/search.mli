(** Choosing a move for the side to move, [Null] on checkmate or stalemate *)
val search : Position.t @ local -> Move.t or_null
