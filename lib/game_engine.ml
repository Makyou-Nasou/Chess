open Player
open Board
open Global

type game = {
  current_player_indice : int;
  players : player list;
  board : board;
}

let pp_game fmt game = pp_board fmt game.board

let init_game strategy_white strategy_black =
  {
    current_player_indice = 0;
    players =
      [ init_player White strategy_white; init_player Black strategy_black ];
    board = init_board ();
  }

let set_next_player_from_game g =
  { g with current_player_indice = 1 - g.current_player_indice }

let get_current_player_from_game g = List.nth g.players g.current_player_indice

let get_next_player_from_game g =
  List.nth g.players (1 - g.current_player_indice)

let get_board_from_game g = g.board

let set_move_played g m =
  {
    g with
    players =
      List.init 2 (fun i ->
          let p = List.nth g.players i in
          if i = g.current_player_indice then set_last_move_to_player p m else p);
  }

let play_move (g : game) (m : move) =
  let next_player = get_next_player_from_game g in
  match
    Board.play_move g.board (get_current_player_from_game g) m next_player
  with
  | None -> None
  | Some b -> Some (set_move_played { g with board = b } m)

exception No_King

let end_of_game game =
  match chess_mate game.board White with
  | Some true -> Some White
  | Some false -> (
      match chess_mate game.board Black with
      | Some true -> Some Black
      | Some false -> None
      | None ->
          let () =
            Format.fprintf Format.std_formatter "No king on the board. "
          in
          raise No_King)
  | None ->
      let () = Format.fprintf Format.std_formatter "No king on the board. " in
      raise No_King

let start_game strategy_white strategy_black =
  let rec aux game nbr_try =
    if nbr_try = 0 then
      let () = Format.fprintf Format.std_formatter "You have tried 3 attempts ...@ " in
      Some (Winner (get_color_from_player (get_next_player_from_game game)))
    else
      let () = pp_game Format.std_formatter game in
      let current_player = get_current_player_from_game game in
      let () =
        Format.fprintf Format.std_formatter "It is the turn of the %s.@ "
          (match get_color_from_player current_player with
          | White -> "white"
          | Black -> "black")
      in
      let mv =
        (get_choose_move_from_player current_player)
          (get_value_of_board game.board)
      in
      match mv with
      | Give_Up ->
          Some (Winner (get_color_from_player (get_next_player_from_game game)))
      | Propose_Draw ->
          let () =
            Format.fprintf Format.std_formatter
              "%s your opponent offers you a draw.@ "
              (match get_color_from_player (get_next_player_from_game game) with
              | White -> "White"
              | Black -> "Black")
          in
          if
            (get_choose_accept_draw (get_next_player_from_game game))
              (get_value_of_board game.board)
          then Some Draw
          else aux game (nbr_try - 1)
      | mv -> (
          match play_move game mv with
          | Some game -> (
              let new_game = set_next_player_from_game game in
              match try end_of_game new_game with No_King -> None with
              | Some t -> Some (Winner t)
              | None ->
                  if
                    stalemate game.board
                      (get_current_player_from_game game)
                      (get_next_player_from_game game)
                  then Some Draw
                  else aux new_game 3)
          | None -> aux game (nbr_try - 1))
  in
  aux (init_game strategy_white strategy_black) 3
