(** Forsyth-Edwards Notation *)

(** FEN of [Board.t] *)
val of_board : Board.t -> string

(** Converts FEN to [Board.t] *)
val to_board_exn : string -> Board.t

val of_position : Position.t @ local -> string

(** TODO: [Position.t] is boxed so it can be in a [result], may need to update *)
val to_position : string -> (Position.t, string) result
