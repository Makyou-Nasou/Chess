open Chess.Global
open Chess.Player
open Chess.Game_engine

let () =
  let str_final =
    match start_game default_choose_move default_choose_move with
    | Some White -> "Winner : White"
    | Some Black -> "Winner : Black"
    | None -> "End of game ..."
  in
  Format.fprintf Format.std_formatter "%s\n" str_final
