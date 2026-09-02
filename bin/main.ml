open Core
open Oxcaml_chess

let () =
  Uci.run
    ~read_line:(fun () -> In_channel.input_line In_channel.stdin)
    ~write_line:print_endline
    ~choose:(fun (position @ local) ~depth ->
      let #{ Search.score; move; nodes } = Search.search position ~depth in
      #{ Uci.score; move; nodes })
;;
