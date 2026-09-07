(** A move, packed into a single immediate

    NOTE: Originally [move] was planned to be an unboxed record of its components, which
    would be more typesafe. However, the record isn't a [value] and I'd like to use some
    arrays of moves and other nice data structures, and this immediate representation is
    much smaller. If I become really bogged down with bugs though... *)

type kind =
  | Normal
  | Double_push
  | En_passant
  | Castle
[@@deriving equal]

type t : immediate

(* Field Accesors *)

val from : t -> Square.t [@@zero_alloc strict]
val to_ : t -> Square.t [@@zero_alloc strict]
val moved : t -> Piece.Kind.t [@@zero_alloc strict]
val kind : t -> kind [@@zero_alloc strict]
val captured : t -> Piece.Kind.t or_null [@@zero_alloc strict]
val promotion : t -> Piece.Kind.t or_null [@@zero_alloc strict]

(** Where the captured piece is *)
val captured_square : t -> Square.t
[@@zero_alloc strict]

(** The rook's origin and destination, ONLY use when [kind] is [Castle] *)
val castle_rook : t -> #(Square.t * Square.t)
[@@zero_alloc strict]

(* Constructors *)

val quiet : moved:Piece.Kind.t -> from:Square.t -> to_:Square.t -> t [@@zero_alloc strict]

val capture
  :  moved:Piece.Kind.t
  -> captured:Piece.Kind.t
  -> from:Square.t
  -> to_:Square.t
  -> t
[@@zero_alloc strict]

(** The destination is derived, undefined for any invalid [from] *)
val double_push : from:Square.t -> t
[@@zero_alloc strict]

(** The captured pawn is neither [from] nor [to] *)
val en_passant : from:Square.t -> to_:Square.t -> t
[@@zero_alloc strict]

val castle : color:Piece.Color.t -> side:Castling.Side.t -> t [@@zero_alloc strict]

(** Covers a promotion with or without a capture *)
val promote
  :  to_kind:Piece.Kind.t
  -> captured:Piece.Kind.t or_null
  -> from:Square.t
  -> to_:Square.t
  -> t
[@@zero_alloc strict]

val is_capture : t -> bool [@@zero_alloc strict]
val equal : t -> t -> bool [@@zero_alloc strict]

(** UCI syntax long algebraic string output, e.g. ["e2e4"], ["a7a8q"] *)
val to_string : t -> string

(** Standard algebraic notation, e.g. ["Nf3"], ["exd5"], ["O-O"] *)
val san : t -> string

(** The listed moves drawn on one board, visualized

    - Mover's FEN letter on its origin, cased by [color]
    - [*] on its destination
    - [x] on a pawn taken en passant

    NOTE: [color] is a parameter because a [t] does not record it; the moves are assumed
    to be one side's. *)
val diagram : color:Piece.Color.t -> t list -> string
