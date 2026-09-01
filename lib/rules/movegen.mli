(** Pseudolegal move generation *)

open Chess_primitives

(** A move buffer and how much of it is filled *)
module Movelist : sig
  type t : value & value & value

  (** Allocates the array, caller is expected to hold one and reuse it. *)
  val create : unit -> t @ unique

  (** How many moves are left to visit. Aliased [t], so this is after movegen is over *)
  val length : t -> int
  [@@zero_alloc strict]

  (** The next move and the moves after it, [Null] once exhausted *)
  val pop : t -> #(Move.t or_null * t)
  [@@zero_alloc strict]
end

(** Pseudolegal moves side to move, written from the start of [moves], caller is expected
    to apply the move then reject it with [Attacks.in_check], for example *)
val generate : Position.t @ local -> Movelist.t @ unique -> Movelist.t @ unique
[@@zero_alloc strict]

(** Find move associated with UCI move at given position *)
val find : Position.t @ local -> string -> Move.t or_null
