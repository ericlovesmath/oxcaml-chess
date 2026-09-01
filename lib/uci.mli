(** The Universal Chess Interface *)

(** UCI Engine *)
val run
  :  read_line:(unit -> string option)
  -> write_line:(string -> unit)
  -> choose:(Position.t @ local -> Move.t or_null)
  -> unit
