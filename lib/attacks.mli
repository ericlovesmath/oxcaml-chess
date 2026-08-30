(** The squares a piece attacks *)

val knight : Square.t -> Bitboard.t [@@zero_alloc strict]
val king : Square.t -> Bitboard.t [@@zero_alloc strict]

(** NOTE: Takes a set rather than a square because move generation steps every pawn at
    once and recovers each origin by stepping back *)
val pawn : Piece.Color.t -> Bitboard.t -> Bitboard.t
[@@zero_alloc strict]

val bishop : occupancy:Bitboard.t -> Square.t -> Bitboard.t [@@zero_alloc strict]
val rook : occupancy:Bitboard.t -> Square.t -> Bitboard.t [@@zero_alloc strict]
val queen : occupancy:Bitboard.t -> Square.t -> Bitboard.t [@@zero_alloc strict]

(** Checks if some [by] color piece attacks [square] *)
val is_attacked : Board.t -> Square.t -> by:Piece.Color.t -> bool
[@@zero_alloc strict]

(** [color]'s king stands on an attacked square *)
val in_check : Board.t -> Piece.Color.t -> bool
[@@zero_alloc strict]
