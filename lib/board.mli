open Global
open Player

type board

val init_board : unit -> board
val pp_board : Format.formatter -> board -> unit
val move : board -> player -> move -> board option
