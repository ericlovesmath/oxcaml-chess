(** The Universal Chess Interface *)

open Chess_primitives
open Chess_rules

(** Search result *)
type report =
  #{ score : int (** Centipawns, positive for the side to move *)
   ; move : Move.t or_null (** [Null] on checkmate or stalemate *)
   ; nodes : int (** Positions visited (including leaves) *)
   }

(** UCI Engine *)
val run
  :  read_line:(unit -> string option)
  -> write_line:(string -> unit)
  -> choose:(Position.t @ local -> depth:int -> report)
  -> unit
