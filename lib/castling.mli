(** The castling rights *)

module Side : sig
  type t =
    | Kingside
    | Queenside
  [@@deriving enumerate, equal]
end

type t = private int

val none : t
val all : t
val mem : t -> Piece.Color.t -> Side.t -> bool [@@zero_alloc strict]
val add : t -> Piece.Color.t -> Side.t -> t [@@zero_alloc strict]
val remove : t -> Piece.Color.t -> Side.t -> t [@@zero_alloc strict]

(** Revokes both of [color]'s rights, as any king move does *)
val remove_color : t -> Piece.Color.t -> t
[@@zero_alloc strict]

val equal : t -> t -> bool [@@zero_alloc strict]

(** FEN field ["KQkq"] or ["-"] *)
val to_string : t -> string

val of_string : string -> t option
