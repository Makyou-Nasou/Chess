open Piece
open Global
open Player

type board

exception No_King

val init_board : unit -> board
val pp_board : Format.formatter -> board -> unit
val get_piece : board -> coordinates -> piece option
val get_board_from_board : board -> piece option list list
val empty_straight : board -> coordinates -> coordinates -> bool
val empty_diagonal : board -> coordinates -> coordinates -> bool
val play_move : board -> player -> move -> player -> board option
val chess_mate : board -> color -> bool
val stalemate : board -> player -> player -> bool
val equals_boards : board -> piece option list list -> bool
