open Piece
open Global
open Player

type board = piece option list list
type move = Movement of coordonne * coordonne | Small_Castling | Big_Castling

val init_board : unit -> board
val pp_board : Format.formatter -> board -> unit
val classic_move : board -> player -> move -> board option
