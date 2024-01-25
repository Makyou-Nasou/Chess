open Piece

type board = piece option list list
type coordonne = int * int

val init_board : unit -> board
val pp_board : Format.formatter -> board -> unit
