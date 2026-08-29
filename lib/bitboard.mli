(** Unboxed bitboard, representing a set of squares on the board *)

type t = int64#

val empty : t
val full : t

(** Bitboard with only [square] set *)
val of_square : Square.t -> t [@@zero_alloc strict]

(** [of_squares squares] is the set of the listed squares *)
val of_squares : Square.t list -> t
[@@zero_alloc strict]

(** Checks if [square] is set in bitboard [t] *)
val mem : t -> Square.t -> bool
[@@zero_alloc strict]

(** Sets bit [square] on bitboard [t] *)
val set : t -> Square.t -> t [@@zero_alloc strict]

(** Unsets bit [square] on bitboard [t] *)
val unset : t -> Square.t -> t
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
val lowest_square : t -> Square.t
[@@zero_alloc strict]

(** Member nearest to h8 ([most significant bit]), does not check empty *)
val highest_square : t -> Square.t
[@@zero_alloc strict]

(** Every square on rank [r]. NOTE: No bounds check, assumes [0 <= r < 8] *)
val rank_mask : int -> t
[@@zero_alloc strict]

(** Renders the board as eight ranks, rank 8 first, ['x'] for a member square and ['.']
    for an empty one, with rank and file legends. There is no trailing newline, so
    [print_endline (to_string t)] prints exactly nine lines. Debugging aid: cold path,
    allocates. *)
val to_string : t -> string
