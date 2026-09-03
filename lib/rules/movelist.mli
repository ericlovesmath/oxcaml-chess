(** A Buffer of pseudolegal moves, only on the local stack

    Writing and reading are separate types. [build] hands a [builder] to a callback and
    then seals it into a readable [t]. Since that is the only way to obtain a [t], and the
    builder does not outlive the callback, nothing can write to a list being read. *)

open Chess_primitives

(** Readable movelist *)
type t

(** Writable movelist *)
type builder

(** Runs [f] against a fresh buffer and seals the result.

    NOTE: No [zero_alloc] annotation, this allocates on the local stack, not the heap, but
    the checker counts [Array.create_local]'s external call as an allocation.
    [test/test_bench.ml] is used to actually guard this.

    TODO: This feels like a potential bug of some kind in OxCaml, check later? *)
val build : (builder @ local -> unit) @ local -> t @ local unique

(** Appends a move, undefined past the buffer's capacity (which is more than the
    theoretical limit of possible moves per position). Do not use this outside of
    [movegen] if possible, or justify strongly if so. *)
val push : builder @ local -> Move.t -> unit
[@@zero_alloc strict]

(** How many moves were generated *)
val length : t @ local -> int [@@zero_alloc strict]

(** The move at [i]. Raises if [i] is outside \[0, [length]), which would otherwise read a
    stale move left over in the buffer. *)
val get : t @ local -> int -> Move.t
[@@zero_alloc strict]

(** First move satisfying [f] *)
val find : t @ local -> f:(Move.t -> bool) @ local -> Move.t or_null

(** Returns if any move satisfies [f] *)
val exists : t @ local -> f:(Move.t -> bool) @ local -> bool

(** Stable sort, with highest [score] first *)
val sorted : t @ local unique -> score:(Move.t -> int) -> t @ local
