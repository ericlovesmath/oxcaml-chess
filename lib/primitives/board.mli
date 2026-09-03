(** The placement of pieces on the board, kind-major layout *)

(* NOTE: [private] just to avoid having to write out kind annotation *)
type t = private
  #{ pawns : Bitboard.t
   ; knights : Bitboard.t
   ; bishops : Bitboard.t
   ; rooks : Bitboard.t
   ; queens : Bitboard.t
   ; kings : Bitboard.t
   ; white : Bitboard.t
   ; black : Bitboard.t
   ; occupancy : Bitboard.t
   }

val empty : t

(** Squares holding a piece of [kind] *)
val kind_board : t -> Piece.Kind.t -> Bitboard.t
[@@zero_alloc strict]

(** Squares holding a piece of [color] *)
val color_board : t -> Piece.Color.t -> Bitboard.t
[@@zero_alloc strict]

(** squares holding exactly [piece] *)
val piece_board : t -> Piece.t -> Bitboard.t
[@@zero_alloc strict]

(** All occupied squares *)
val occupancy : t -> Bitboard.t [@@zero_alloc strict]

val kind_at : t -> Square.t -> Piece.Kind.t or_null [@@zero_alloc strict]
val color_at : t -> Square.t -> Piece.Color.t or_null [@@zero_alloc strict]
val king_square : t -> Piece.Color.t -> Square.t [@@zero_alloc strict]
val toggle_piece : t -> Piece.t -> Square.t -> t [@@zero_alloc strict]

(** Moves [piece] in a single pass over the three affected boards *)
val move_piece : t -> Piece.t -> from:Square.t -> to_:Square.t -> t
[@@zero_alloc strict]

(** Checks that the cached aggregates agree with the six kind boards *)
val invariant : t -> unit

val to_string : t -> string

(** The standard opening position *)
val start : t
