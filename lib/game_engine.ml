open Player
open Board
open Global
open Piece

type game = {
  current_player_indice : int;
  players : player list;
  board : board;
  fifty_moves : int;
  previous_position : piece option list list list;
}

let pp_game fmt game = pp_board fmt game.board

let init_game strategy_white strategy_black =
  {
    current_player_indice = 0;
    players =
      [ init_player White strategy_white; init_player Black strategy_black ];
    board = init_board ();
    fifty_moves = 0;
    previous_position = [];
  }

let set_next_player_from_game g =
  { g with current_player_indice = 1 - g.current_player_indice }

let get_current_player_from_game g = List.nth g.players g.current_player_indice

let get_next_player_from_game g =
  List.nth g.players (1 - g.current_player_indice)

let get_board_from_game g = g.board

let play_move (g : game) (m : move) =
  let current_player = get_current_player_from_game g in
  let fifty_rule =
    match m with
    | Movement (coord_start, coord_final) -> (
        (match get_piece g.board coord_start with
        | Some { shape = Pawn _; color = _ } -> true
        | _ -> false)
        ||
        match get_piece g.board coord_final with
        | Some p -> p.color <> get_color_from_player current_player
        | None -> false)
    | _ -> false
  in
  Board.play_move g.board current_player m (get_next_player_from_game g)
  |> Option.map (fun b ->
         let fifty_moves = if fifty_rule then 0 else g.fifty_moves + 1 in
         {
           g with
           fifty_moves;
           board = b;
           previous_position =
             get_board_from_board g.board :: g.previous_position;
         })

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

let threefold_repetitions g =
  List.fold_left
    (fun acc e -> if equals_boards g.board e then acc + 1 else acc)
    0 g.previous_position
  >= 3

let start_game strategy_white strategy_black =
  let rec aux game nbr_try =
    if nbr_try = 0 then
      let () = pp_game Format.std_formatter game in
      let () =
        Format.fprintf Format.std_formatter "You have tried 3 attempts ...@ "
      in
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
          (get_board_from_board game.board)
      in
      match mv with
      | Give_Up ->
          let () = pp_game Format.std_formatter game in
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
              (get_board_from_board game.board)
          then
            let () = pp_game Format.std_formatter game in
            Some Draw
          else aux game (nbr_try - 1)
      | mv -> (
          match play_move game mv with
          | Some game -> (
              let game = set_next_player_from_game game in
              match try end_of_game game with No_King -> None with
              | Some t ->
                  let () = pp_game Format.std_formatter game in
                  Some (Winner t)
              | None ->
                  if threefold_repetitions game then
                    let () = pp_game Format.std_formatter game in
                    let () =
                      Format.fprintf Format.std_formatter
                        "More than 3 repetitions.@ "
                    in
                    Some Draw
                  else if game.fifty_moves > 50 then
                    let () = pp_game Format.std_formatter game in
                    let () =
                      Format.fprintf Format.std_formatter
                        "More than 50 moves without moving pawns or eating \
                         enemy piece.@ "
                    in
                    Some Draw
                  else if
                    stalemate game.board
                      (get_current_player_from_game game)
                      (get_next_player_from_game game)
                  then
                    let () = pp_game Format.std_formatter game in
                    let () =
                      Format.fprintf Format.std_formatter "Stalemate.@ "
                    in
                    Some Draw
                  else aux game 3)
          | None -> aux game (nbr_try - 1))
  in
  aux (init_game strategy_white strategy_black) 3
