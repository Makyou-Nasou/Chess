open Chess.Global
open Chess.Player
open Chess.Game_engine

let () =
  let str_final =
    match start_game (default_strategy ()) (default_strategy ()) with
    | Some (Winner White) -> "Winner : White"
    | Some (Winner Black) -> "Winner : Black"
    | Some Draw -> "Draw"
    | None -> "End of game ... Technical problem ..."
  in
  Format.fprintf Format.std_formatter "%s\n" str_final
