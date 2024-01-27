open Piece
open Global

type board

val init_board : unit -> board
val pp_board : Format.formatter -> board -> unit
val get_piece : board -> coordinates -> piece option
val empty_straight : board -> coordinates -> coordinates -> bool
val empty_diagonal : board -> coordinates -> coordinates -> bool
val move : board -> color -> color -> move option -> move -> board option
val chess_mate : board -> color -> bool option
