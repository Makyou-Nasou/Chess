open Player
open Global
open Board

type game
type game_status = Continue_game of game | Draw_game of game | Game_error
type final_status = Winner of color | Draw | Error of string

(*Method that initializes a game, with all its attributes. Including its board and players.*)
val init_game : strategy -> strategy -> game

(*Method that displays a game (its board).*)
val pp_game : Format.formatter -> game -> unit

(*Gives the player who must play*)
val get_current_player_from_game : game -> player

(*Gives the player who must play the next turn*)
val get_next_player_from_game : game -> player

(*Returns the board of the game*)
val get_board_from_game : game -> board

(*Play the current player’s turn.*)
val play_move : game -> move -> game_status option

(*Play a full game in a terminal. Take the statégies of both players.*)
val start_game : strategy -> strategy -> final_status
