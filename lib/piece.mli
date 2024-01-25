type shape = King | Queen | Rook | Bishop | Horse | Pawn
type color = Black | White
type piece = { shape : shape; color : color }

val pp_piece : Format.formatter -> piece -> unit
