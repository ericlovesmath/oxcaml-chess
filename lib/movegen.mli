(** Pseudolegal move generation *)

(** A move buffer and how much of it is filled *)
module Movelist : sig
  type t : value & value

  (** Allocates the array, caller is expected to hold one and reuse it. *)
  val create : unit -> t @ unique

  (** Aliased [t], so this is after movegen is over *)
  val length : t -> int
  [@@zero_alloc strict]

  (** Aliased [t], so this is after movegen is over. NOTE: no bounds check *)
  val get : t -> int -> Move.t
  [@@zero_alloc strict]
end

(** Pseudolegal moves side to move, written from the start of [moves], caller is expected
    to apply the move then reject it with [Attacks.in_check], for example *)
val generate : Position.t @ local -> Movelist.t @ unique -> Movelist.t @ unique
[@@zero_alloc strict]
