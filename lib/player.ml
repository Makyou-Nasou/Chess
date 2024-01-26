open Global

type player = { color : color; last_move : move option }

let init_player c = { color = c; last_move = None }
let get_color_from_player (p : player) = p.color
let get_last_move_from_player (p : player) = p.last_move
