open Chess.Global
open Chess.Player
open Chess.Game_engine

let () =
  let str_final =
    match start_game (default_strategy ()) (default_strategy ()) with
    | Winner White -> "Winner : White"
    | Winner Black -> "Winner : Black"
    | Draw -> "Draw"
    | Error s ->
        let () = Format.fprintf Format.std_formatter "%s@ " s in
        " @ End of game ... Technical problem ..."
  in
  Format.fprintf Format.std_formatter "%s\n" str_final
