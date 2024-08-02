open Chess.Board

let generator_board () =
  let file = open_in "FEN_list" in
  let random_index = Random.int 1525144 in
  let rec find_line i =
    let line = input_line file in
    if i = random_index then line else find_line (i + 1)
  in
  generate_board_with_fen (find_line 0)
