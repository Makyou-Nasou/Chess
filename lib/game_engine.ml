open Player
open Board
open Global
open Piece

type game = {
  players : player list;
  board : board;
  previous_position : piece option list list list;
}

type game_status = Continue_game of game | Draw_game of game | Game_error
type final_status = Winner of color | Draw | Error of string

let pp_game fmt game = pp_board fmt game.board

let init_game strategy_white strategy_black =
  {
    players =
      [ init_player White strategy_white; init_player Black strategy_black ];
    board = init_board ();
    previous_position = [];
  }

let get_current_player_from_game g =
  List.nth g.players
    (match get_current_player_from_board g.board with White -> 0 | Black -> 1)

let get_next_player_from_game g =
  List.nth g.players
    (match get_current_player_from_board g.board with White -> 1 | Black -> 0)

let get_board_from_game g = g.board

let play_move (g : game) (m : move) : game_status option =
  let current_player = get_current_player_from_game g in
  Board.play_move g.board (get_choose_promotion current_player) m
  |> Option.map (fun s ->
         let g =
           {
             g with
             previous_position =
               get_board_from_board g.board :: g.previous_position;
           }
         in
         match s with
         | Draw_board b -> Draw_game { g with board = b }
         | Continue_board b -> Continue_game { g with board = b }
         | _ -> Game_error)

let end_of_game game =
  match chess_mate game.board White with
  | true -> Some Black
  | false -> (
      match chess_mate game.board Black with
      | true -> Some White
      | false -> None)

let threefold_repetitions g =
  List.fold_left
    (fun acc e -> if equals_boards g.board e then acc + 1 else acc)
    0 g.previous_position
  >= 3

let start_game strategy_white strategy_black =
  Random.self_init ();
  let rec aux game nbr_try =
    let current_player = get_current_player_from_game game in
    let next_player = get_next_player_from_game game in
    if nbr_try = 0 then
      let () = pp_game Format.std_formatter game in
      let () =
        Format.fprintf Format.std_formatter "You have tried 3 attempts ...@ "
      in
      Winner (get_color_from_player next_player)
    else
      let () = pp_game Format.std_formatter game in
      let () =
        Format.fprintf Format.std_formatter "It is the turn of the %s.@ "
          (match get_color_from_player current_player with
          | White -> "white"
          | Black -> "black")
      in
      let mv = (get_choose_move_from_player current_player) game.board in
      match mv with
      | Give_Up ->
          let () = pp_game Format.std_formatter game in
          Winner (get_color_from_player next_player)
      | Propose_Draw ->
          let () =
            Format.fprintf Format.std_formatter
              "%s your opponent offers you a draw.@ "
              (match get_color_from_player next_player with
              | White -> "White"
              | Black -> "Black")
          in
          if (get_choose_accept_draw next_player) game.board then
            let () = pp_game Format.std_formatter game in
            Draw
          else aux game (nbr_try - 1)
      | mv -> (
          match play_move game mv with
          | Some (Draw_game game) ->
              let () = pp_game Format.std_formatter game in
              let () =
                Format.fprintf Format.std_formatter
                  "More than 50 moves without moving pawns or eating enemy \
                   piece.@ "
              in
              Draw
          | Some (Continue_game game) -> (
              match end_of_game game with
              | Some t ->
                  let () = pp_game Format.std_formatter game in
                  Winner t
              | None ->
                  if threefold_repetitions game then
                    let () = pp_game Format.std_formatter game in
                    let () =
                      Format.fprintf Format.std_formatter
                        "More than 3 repetitions.@ "
                    in
                    Draw
                  else if stalemate game.board then
                    let () = pp_game Format.std_formatter game in
                    let () =
                      Format.fprintf Format.std_formatter "Stalemate.@ "
                    in
                    Draw
                  else aux game 3)
          | Some Game_error -> Error "Play_move produces an error."
          | None -> aux game (nbr_try - 1))
  in
  try aux (init_game strategy_white strategy_black) 3
  with No_King -> Error "No king on the board."
