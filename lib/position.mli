(** Complete game state *)

(* NOTE: If unboxing [Position.t] is significantly more efficient, we will do so, but
   right now I'm running into too many OxCaml issues that I don't care to fix. I think
   just using [local] allocations for everything will be more than enough. *)

type t

val start : t

(** Creates [Position.t], raises failure if [invariant] fails *)
val create_exn
  :  board:Board.t
  -> to_move:Piece.Color.t
  -> castling:Castling.t
  -> en_passant:Square.t or_null
  -> halfmove_clock:int
  -> fullmove_number:int
  -> t

val board : t @ local -> Board.t [@@zero_alloc strict]
val to_move : t @ local -> Piece.Color.t [@@zero_alloc strict]
val castling : t @ local -> Castling.t [@@zero_alloc strict]

(** The square a capturing pawn moves [i] to, *not* the square of the pawn it captures *)
val en_passant : t @ local -> Square.t or_null
[@@zero_alloc strict]

val halfmove_clock : t @ local -> int [@@zero_alloc strict]
val fullmove_number : t @ local -> int [@@zero_alloc strict]

(** Validates legal game state. NOTE: No checks for checked positions, incomplete

    TODO: [Attacks.in_check] now makes the missing check writable - the side *not* to move
    must not be in check, or the previous move was illegal. [Attacks] depends only on
    [Board], so [Position] may use it without a cycle. Deferred because several existing
    test positions have not been audited against it. *)
val invariant : t @ local -> unit

(** Is side to move in check? *)
val in_check : t @ local -> bool [@@zero_alloc strict]

(** Did the side that just moved leave its own king attacked? (Illegal move) *)
val mover_in_check : t @ local -> bool
[@@zero_alloc strict]

(** Applies [move], no checking invariants *)
val make_move : t @ local -> Move.t -> t @ local

val to_string : t @ local -> string
