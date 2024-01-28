open Piece
open Global
open Player

type board

val init_board : unit -> board
val pp_board : Format.formatter -> board -> unit
val get_piece : board -> coordinates -> piece option
val empty_straight : board -> coordinates -> coordinates -> bool
val empty_diagonal : board -> coordinates -> coordinates -> bool
val play_move : board -> player -> move -> player -> bool
val chess_mate : board -> color -> bool option
val get_value_of_board : board -> piece option array array
val stalemate : board -> player -> player -> bool
