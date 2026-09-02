(** Positions shared tests and benchmarks *)

(* Perft Corpus taken from [https://chessprogramming.org/Perft_Results] *)

(** Starting chess position *)
val startpos : string

(** Open position with castling and en passant *)
val kiwipete : string

(** Pawn endgame, promotions and checks at shallow depth *)
val endgame : string

(** Promotion-heavy and mirrored *)
val promotions : string

(** Edge cases of buggy castling and en passant legality *)
val tricky : string

(** Middlegame example *)
val midgame : string

(* Handwritten examples *)

(** Kings and rooks alone on the back rank *)
val castling_back_rank : string

(** The piece-placement field alone, for board-level round trips *)
val placement : string -> string
