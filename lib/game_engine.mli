open Player
open Global

type game

val init_game : unit -> game
val pp_game : Format.formatter -> game -> unit
val get_current_player_from_game : game -> player
val get_next_player_from_game : game -> player
val move : game -> move -> game option
