(** Unboxed colored piece *)

module Color : sig
  type t =
    | White
    | Black
  [@@deriving enumerate, equal, string]

  val flip : t -> t [@@zero_alloc strict]
end

module Kind : sig
  type t =
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King
  [@@deriving enumerate, equal]

  (** Lowercase FEN letter ['pnbrqk'] *)
  val to_char : t -> char [@@zero_alloc strict]

  (** Case-insensitive, [None] for invalid FEN *)
  val of_char : char -> t option

  (** [Pawn = 0] up to [King = 5] *)
  val to_index : t -> int [@@zero_alloc strict]

  (** NOTE: Does not bounds check *)
  val unsafe_of_index : int -> t [@@zero_alloc strict]
end

type t =
  #{ color : Color.t
   ; kind : Kind.t
   }

(** FEN letter, uppercase for White, lowercase for Black *)
val to_char : t -> char
[@@zero_alloc strict]
