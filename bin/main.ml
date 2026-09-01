open Core
open Oxcaml_chess

let () =
  Uci.run
    ~read_line:(fun () -> In_channel.input_line In_channel.stdin)
    ~write_line:(fun line ->
      Out_channel.output_string Out_channel.stdout (line ^ "\n");
      (* Without this a GUI hangs on a reply sitting in the buffer. *)
      Out_channel.flush Out_channel.stdout)
    ~choose:Search.search
;;
