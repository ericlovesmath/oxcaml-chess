open Chess_primitives
open Chess_rules

(** Search result *)
type result =
  #{ score : int (** Centipawns, positive for the side to move *)
   ; move : Move.t or_null (** [Null] on checkmate or stalemate *)
   ; nodes : int (** Positions visited (including leaves) *)
   ; leaves : int (** Leaf positions in search tree *)
   }

(** Choosing a move for the side to move *)
val search : Position.t @ local -> depth:int -> result
