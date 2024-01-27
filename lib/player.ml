open Global
open Board

type player = {
  color : color;
  last_move : move option;
  choose_move : board -> move option;
}

let init_player choose_move c = { color = c; last_move = None; choose_move }
let get_color_from_player (p : player) = p.color
let get_last_move_from_player (p : player) = p.last_move
let get_choose_move_from_player (p : player) = p.choose_move

let convert_coordinates (coord : string) : (int * int) * (int * int) =
  let convert_char c = Char.code c - Char.code 'a' in
  let convert_chiffre c = Char.code '8' - Char.code c in
  let y1 = convert_char coord.[0] in
  let x1 = convert_chiffre coord.[1] in
  let y2 = convert_char coord.[3] in
  let x2 = convert_chiffre coord.[4] in
  ((x1, y1), (x2, y2))

let rec request question =
  Format.printf "%s (write `exit` to quit):@ " question;
  Format.print_flush ();
  try
    Scanf.scanf "%s@\n" (fun s ->
        if s = "exit" then (
          Format.printf "You've given up. Come and play again another day!@;";
          None)
        else if s = "bc" then Some Big_Castling
        else if s = "sc" then Some Small_Castling
        else if String.length s <> 5 || s.[2] <> ' ' then (
          Format.printf "Invalid entry.@;";
          request question)
        else
          let (x1, y1), (x2, y2) = convert_coordinates s in
          if
            x1 < 0 || x1 > 7 || y1 < 0 || y1 > 7 || x2 < 0 || x2 > 7 || y2 < 0
            || y2 > 7
          then (
            Format.printf "Invalid entry.@;";
            request question)
          else Some (Movement ((x1, y1), (x2, y2))))
  with Failure _ ->
    Format.printf "Invalid entry.@;";
    request question

let default_choose_move _ =
  request "Choose move on format : start finish (example : a2 a3)"
