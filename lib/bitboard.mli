(** Unboxed bitboard, representing a set of squares on the board. Bit [i] is set iff
    square [i] is a member, where [i = rank * 8 + file] *)

type t = int64#

(* TODO: Make these type safe *)
type square = int

val empty : t
val full : t

(** Bit index of a square, e.g. [square ~rank:3 ~file:4] is e4 *)
val square : rank:int -> file:int -> square
[@@zero_alloc strict]

(** [rank_of (square ~rank ~file) = rank] *)
val rank_of : square -> int
[@@zero_alloc strict]

(** [file_of (square ~rank ~file) = file] *)
val file_of : square -> int
[@@zero_alloc strict]

(** Bitboard with only [square] set *)
val of_square : square -> t [@@zero_alloc strict]

(** [of_squares squares] is the set of the listed indices. Convenience for setting up a
    board by hand; the list itself is a [value], so this is a cold path. *)
val of_squares : square list -> t
[@@zero_alloc strict]

(** Checks if [square] is set in bitboard [t] *)
val mem : t -> square -> bool
[@@zero_alloc strict]

(** Sets bit [square] on bitboard [t] *)
val set : t -> square -> t [@@zero_alloc strict]

(** Unsets bit [square] on bitboard [t] *)
val unset : t -> square -> t
[@@zero_alloc strict]

(** Intersection *)
val ( land ) : t -> t -> t [@@zero_alloc strict]

(** Union *)
val ( lor ) : t -> t -> t [@@zero_alloc strict]

(** Symmetric difference *)
val ( lxor ) : t -> t -> t [@@zero_alloc strict]

(** Difference *)
val ( - ) : t -> t -> t [@@zero_alloc strict]

(** Complement *)
val lnot : t -> t [@@zero_alloc strict]

val equal : t -> t -> bool [@@zero_alloc strict]
val is_empty : t -> bool [@@zero_alloc strict]

(** Number of squares in board ([popcnt]) *)
val count : t -> int [@@zero_alloc strict]

(** Member nearest to a1 (least significant bit), does not check empty *)
val lowest_square : t -> square
[@@zero_alloc strict]

(** Member nearest to h8 ([most significant bit]), does not check empty *)
val highest_square : t -> square
[@@zero_alloc strict]

(** Renders the board as eight ranks, rank 8 first, ['x'] for a member square and ['.']
    for an empty one, with rank and file legends. There is no trailing newline, so
    [print_endline (to_string t)] prints exactly nine lines. Debugging aid: cold path,
    allocates. *)
val to_string : t -> string
