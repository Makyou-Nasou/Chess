open Piece
open Global

type board

type board_status =
  | Continue_board of board
  | Draw_board of board
  | Error_board of string

exception No_King

(*Method that initializes a board with black at the top, white at the bottom.*)
val init_board : unit -> board

(*Method to display the board in a terminal. With the pieces that were removed from the game. As well as the one still in play.*)
val pp_board : Format.formatter -> board -> unit

(*Method that takes a board and a coordinate, and returns the piece present on this coordinate. Be careful, if there is nothing, returns None. And if the coordinates are outside the board raise Invalid_coordinates.*)
val get_piece : board -> coordinates -> piece option

(*Returns the list that represents the board with the pieces.*)
val get_board_from_board : board -> piece option list list

(*Returns the last move that was played on the board.*)
val get_last_move_from_board : board -> move option

(*Return the player who must play*)
val get_current_player_from_board : board -> color

(*Checks if the boxes of the line between the coordinates indicated are empty (coordination indicated excluded). Be careful, if it is not a row or column returns false. If the coordinates are outside the board raise Invalid_coordinates.*)
val empty_straight : board -> coordinates -> coordinates -> bool

(*Checks if the boxes of the diagonal between the coordinates indicated are empty (coordination indicated excluded). Be careful if it is not a diagonal returns false. If the coordinates are outside the board raise Invalid_coordinates.*)
val empty_diagonal : board -> coordinates -> coordinates -> bool

(*Method that take a board, and a function that allows you to choose the promotion of a piece (see choose_promotion in player). Plays the movement of the current player on this board. May raise No_King or Invalid_coordinates, but this implies that the board is broken.*)
val play_move : board -> (board -> shape) -> move -> board_status option

(*Method that takes a board and a move. And checks if the movement allows to apply the rule of "en passant".*)
val can_en_passant : board -> move -> bool

(*Method that takes a board and a color. And checks if this color is chess mate.*)
val chess_mate : board -> color -> bool

(*Method that takes a board. And check if the next player who will play on the board can at least play a valid move. In theory, in a classic game if it’s not, it draws*)
val stalemate : board -> bool

(*Method that takes a board and a board as a list. And checks if the tray model is in the same layout as the list.*)
val equals_boards : board -> piece option list list -> bool
