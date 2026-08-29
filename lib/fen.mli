(** Forsyth-Edwards Notation *)

(** FEN of [Board.t] *)
val of_board : Board.t -> string

(** Converts FEN to [Board.t] *)
val to_board_exn : string -> Board.t
