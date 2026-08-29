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
end

type t =
  #{ color : Color.t
   ; kind : Kind.t
   }

(** FEN letter, uppercase for White, lowercase for Black *)
val to_char : t -> char
[@@zero_alloc strict]
