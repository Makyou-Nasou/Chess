open Player
open Board
open Global

type game = { current_player : int; players : player list; board : board }

let pp_game fmt game = pp_board fmt game.board

let init_game chose_move_black chose_move_white =
  {
    current_player = 0;
    players =
      [ init_player chose_move_black White; init_player chose_move_white Black ];
    board = init_board ();
  }

let set_next_player_from_game g =
  { g with current_player = 1 - g.current_player }

let get_current_player_from_game g = List.nth g.players g.current_player
let get_next_player_from_game g = List.nth g.players (1 - g.current_player)

let move (g : game) (m : move) =
  let next_player = get_next_player_from_game g in
  match
    Board.move g.board
      (get_color_from_player (get_current_player_from_game g))
      (get_color_from_player next_player)
      (get_last_move_from_player next_player)
      m
  with
  | None -> None
  | Some b -> Some { g with board = b }

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

let start_game chose_move_black chose_move_white =
  let rec aux game nbr_try =
    let () = pp_game Format.std_formatter game in
    let current_player = get_current_player_from_game game in
    let () =
      Format.fprintf Format.std_formatter "It is the turn of the %s.@ "
        (match get_color_from_player current_player with
        | White -> "white"
        | Black -> "black")
    in
    let mv = (get_choose_move_from_player current_player) game.board in
    match mv with
    | None -> None
    | Some mv -> (
        match move game mv with
        | Some game -> (
            let new_game = set_next_player_from_game game in
            match try end_of_game new_game with No_King -> None with
            | Some t -> Some t
            | None -> aux new_game 3)
        | None ->
            if nbr_try = 0 then
              let () =
                Format.fprintf Format.std_formatter "More than 3 mistakes ..."
              in
              None
            else aux game (nbr_try - 1))
  in
  aux (init_game chose_move_black chose_move_white) 3
