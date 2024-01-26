open Player
open Board
open Global

type game = { current_player : int; players : player list; board : board }

let pp_game fmt game = pp_board fmt game.board

let init_game () =
  {
    current_player = 0;
    players = [ init_player Black; init_player White ];
    board = init_board ();
  }

let get_current_player_from_game g = List.nth g.players g.current_player
let get_next_player_from_game g = List.nth g.players (1 - g.current_player)

let move (g : game) (m : move) =
  match
    Board.move g.board
      (get_current_player_from_game g)
      (get_next_player_from_game g)
      m
  with
  | None -> None
  | Some b -> Some { g with board = b }

exeption No_king

let end_of_game game = match chess_mate game.board White with 
|Some b when b -> Some White 
|Some b when not b -> (match chess_mate game.board Black with
                            | Some b when b -> Some Black 
                            | _ -> None)
|None -> raise No_king