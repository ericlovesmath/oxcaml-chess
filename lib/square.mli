(** A square on the board, indexed [rank * 8 + file], so a1 = 0, h1 = 7, a8 = 56 and h8
    = 63. This is the indexing [Bitboard] uses for its bits. *)

type t = private int

(** [create ~rank:3 ~file:4] is [e4]. NOTE: No bounds check. *)
val create : rank:int -> file:int -> t
[@@zero_alloc strict]

(** [rank (create ~rank ~file) = rank] *)
val rank : t -> int [@@zero_alloc strict]

(** [file (create ~rank ~file) = file] *)
val file : t -> int [@@zero_alloc strict]

(** NOTE: No bounds check, caller promises [0 <= i < 64] *)
val unsafe_of_int : int -> t
[@@zero_alloc strict]

val equal : t -> t -> bool [@@zero_alloc strict]

(** Algebraic notation, such as ["e4"] *)
val to_string : t -> string

val of_string : string -> t option
val of_string_exn : string -> t
