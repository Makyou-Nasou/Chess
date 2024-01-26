open Piece
open Player
open Global

type board

val init_board : unit -> board
val pp_board : Format.formatter -> board -> unit
val get_piece : board -> coordonne -> piece option
val get_line_from_board : board -> int -> piece option list
val empty_straight : board -> coordonne -> coordonne -> bool
val empty_diagonal : board -> coordonne -> coordonne -> bool
val move : board -> player -> player -> move -> board option
val chess_mate : board -> color -> bool option
