open Global

type shape =
  | King of bool
  | Queen
  | Rook of bool
  | Bishop
  | Horse
  | Pawn of bool

type piece = { shape : shape; color : color }

val pp_piece : Format.formatter -> piece -> unit
