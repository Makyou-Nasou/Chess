open Player
open Global
open Piece
open Board

type game

val init_game :
  (piece option array array -> move option) ->
  (piece option array array -> move option) ->
  game

val pp_game : Format.formatter -> game -> unit
val get_current_player_from_game : game -> player
val get_next_player_from_game : game -> player
val get_board_from_game : game -> board
val play_move : game -> move -> game option

val start_game :
  (piece option array array -> move option) ->
  (piece option array array -> move option) ->
  color option
