open Piece
open Global
open Player

type board = piece option array array

let get_starter_piece ((line, column) : coordinates) =
  assert (is_valide_coordinates (line, column));
  match line with
  | 1 -> Some { shape = Pawn true; color = Black }
  | 6 -> Some { shape = Pawn true; color = White }
  | _ -> (
      let color =
        match line with 0 -> Some Black | 7 -> Some White | _ -> None
      in
      let shape =
        match column with
        | 0 -> Some (Rook true)
        | 1 -> Some Horse
        | 2 -> Some Bishop
        | 3 -> Some Queen
        | 4 -> Some (King true)
        | 5 -> Some Bishop
        | 6 -> Some Horse
        | 7 -> Some (Rook true)
        | _ -> None
      in
      match (color, shape) with
      | None, _ | _, None -> None
      | Some c, Some s -> Some { shape = s; color = c })

let init_board () =
  Array.init 8 (fun line ->
      Array.init 8 (fun column -> get_starter_piece (line, column)))

let pp_board fmt board =
  let () =
    Format.fprintf fmt
      " | a | b | c | d | e | f | g | h@.――――――――――――――――――――――――――――――――――@."
  in
  let print_line line_number =
    let rec aux_print_line column_number =
      if column_number <= 7 then (
        Format.fprintf fmt "| ";
        let p = Array.get (Array.get board line_number) column_number in
        match p with
        | None ->
            Format.fprintf fmt " ";
            Format.fprintf fmt " ";
            aux_print_line (column_number + 1)
        | Some p ->
            pp_piece fmt p;
            Format.fprintf fmt " ";
            aux_print_line (column_number + 1))
      else Format.fprintf fmt "|@.――――――――――――――――――――――――――――――――――@."
    in
    aux_print_line 0
  in
  let rec print_board (i : int) =
    if i <= 7 then (
      Format.fprintf fmt "%i" (8 - i);
      print_line i;
      print_board (i + 1))
    else ()
  in
  print_board 0

let get_piece (b : board) (c : coordinates) =
  if is_valide_coordinates c then
    match c with x, y -> Array.get (Array.get b x) y
  else raise Invalide_coordinates

(*We don’t check whether the starting box or the arriving box are empty.
   We just look at whether the intermediate boxes are empty.*)
let empty_straight (b : board) (coord_start : coordinates)
    (coord_final : coordinates) =
  if is_valide_coordinates coord_start && is_valide_coordinates coord_final then
    match (coord_start, coord_final) with
    | ( (coord_start_line, coord_start_column),
        (coord_final_line, coord_final_column) ) ->
        let distance_line = coord_start_line - coord_final_line in
        let distance_column = coord_start_column - coord_final_column in
        if distance_column <> 0 && distance_line <> 0 then false
        else
          let dir_line =
            if distance_line < 0 then 1 else if distance_line = 0 then 0 else -1
          in
          let dir_column =
            if distance_column < 0 then 1
            else if distance_column = 0 then 0
            else -1
          in
          let rec aux (coord_current_line, coord_current_line_colum) =
            if (coord_current_line, coord_current_line_colum) = coord_final then
              true
            else
              get_piece b (coord_current_line, coord_current_line_colum) = None
              && aux
                   ( coord_current_line + dir_line,
                     coord_current_line_colum + dir_column )
          in
          aux (coord_start_line + dir_line, coord_start_column + dir_column)
  else raise Invalide_coordinates

(*We don’t check whether the starting box or the arriving box are empty.
   We just look at whether the intermediate boxes are empty.*)
let empty_diagonal (b : board) (coord_start : coordinates)
    (coord_final : coordinates) =
  if is_valide_coordinates coord_start && is_valide_coordinates coord_final then
    match (coord_start, coord_final) with
    | ( (coord_start_line, coord_start_column),
        (coord_final_line, coord_final_column) ) ->
        let distance_line = coord_start_line - coord_final_line in
        let distance_column = coord_start_column - coord_final_column in
        if distance_column <> distance_line && distance_column <> -distance_line
        then false
        else
          let dir_line = if distance_line < 0 then 1 else -1 in
          let dir_column = if distance_column < 0 then 1 else -1 in
          let rec aux (coord_current_line, coord_current_line_colum) =
            if (coord_current_line, coord_current_line_colum) = coord_final then
              true
            else
              get_piece b (coord_current_line, coord_current_line_colum) = None
              && aux
                   ( coord_current_line + dir_line,
                     coord_current_line_colum + dir_column )
          in
          aux (coord_start_line + dir_line, coord_start_column + dir_column)
  else raise Invalide_coordinates

(*We assume that the arrival box is empty or occupied by an enemy.
   That the starting box is occupied by the piece given as an argument.*)
let can_move (b : board) (p : piece) (coord_start : coordinates)
    (coord_final : coordinates) =
  if is_valide_coordinates coord_start && is_valide_coordinates coord_final then
    match (coord_start, coord_final) with
    | ( (coord_start_line, coord_start_column),
        (coord_final_line, coord_final_column) ) -> (
        let distance_line = coord_start_line - coord_final_line in
        let distance_column = coord_start_column - coord_final_column in
        match p.shape with
        | King _ ->
            (distance_column = 1 || distance_column = -1 || distance_line = 1
           || distance_line = -1)
            && distance_column <= 1 && distance_column >= -1
            && distance_line <= 1 && distance_line >= -1
        | Queen ->
            empty_diagonal b coord_start coord_final
            || empty_straight b coord_start coord_final
        | Bishop -> empty_diagonal b coord_start coord_final
        | Rook _ -> empty_straight b coord_start coord_final
        | Horse ->
            (distance_line = 2 || distance_line = -2)
            && (distance_column = 1 || distance_column = -1)
            || (distance_line = 1 || distance_line = -1)
               && (distance_column = 2 || distance_column = -2)
        | Pawn first_move ->
            distance_column = 0
            && (((distance_line = 2 && p.color = White)
                || (distance_line = -2 && p.color = Black))
                && first_move
               || (distance_line = 1 && p.color = White)
               || (distance_line = -1 && p.color = Black))
            && get_piece b coord_final = None
            || (distance_column = 1 || distance_column = -1)
               && ((distance_line = 1 && p.color = White)
                  || (distance_line = -1 && p.color = Black))
               && get_piece b coord_final <> None)
  else raise Invalide_coordinates

(*Removes the piece from the starting coordinate.
   Puts the deleted piece on the new coordinate*)
let move_from_coord_to_coord (b : board) (coord_start : coordinates)
    (coord_final : coordinates) =
  assert (is_valide_coordinates coord_start && is_valide_coordinates coord_final);
  match (coord_start, coord_final) with
  | ( (start_line_number, start_column_number),
      (final_line_number, final_column_number) ) ->
      let piece = get_piece b (start_line_number, start_column_number) in
      let start_line = Array.get b start_line_number in
      let () = Array.set start_line start_column_number None in
      let final_line =
        if start_line_number = final_line_number then start_line
        else Array.get b final_line_number
      in
      let () =
        Array.set final_line final_column_number
          (match piece with
          | None -> None
          | Some piece -> (
              match piece.shape with
              | King _ -> Some { piece with shape = King false }
              | Rook _ -> Some { piece with shape = Rook false }
              | Pawn _ -> Some { piece with shape = Pawn false }
              | _ -> Some piece))
      in
      b

let delete_piece (b : board) ((cl, cc) : coordinates) =
  assert (is_valide_coordinates (cl, cc));
  let () = Array.set (Array.get b cl) cc None in
  b

(*p is the piece we want to move*)
let try_passant (b : board) (p : piece) (m : move) (next_player : player) =
  match m with
  | Movement (current_move_coord_start, current_move_coord_final) -> (
      assert (
        is_valide_coordinates current_move_coord_start
        && is_valide_coordinates current_move_coord_final);
      match p.shape with
      | Pawn _ -> (
          match get_last_move_from_player next_player with
          | Some
              (Movement
                ( ( previous_move_coord_start_line,
                    previous_move_coord_start_column ),
                  ( previous_move_coord_final_line,
                    previous_move_coord_final_column ) )) -> (
              assert (
                is_valide_coordinates
                  ( previous_move_coord_start_line,
                    previous_move_coord_start_column )
                && is_valide_coordinates
                     ( previous_move_coord_final_line,
                       previous_move_coord_final_column ));
              match
                get_piece b
                  ( previous_move_coord_final_line,
                    previous_move_coord_final_column )
              with
              | Some piece -> (
                  match piece.shape with
                  | Pawn _ ->
                      let distance_line_previous_move =
                        previous_move_coord_start_line
                        - previous_move_coord_final_line
                      in
                      let distance_column_previous_move =
                        previous_move_coord_start_column
                        - previous_move_coord_final_column
                      in
                      if
                        (distance_line_previous_move = 2
                        || distance_line_previous_move = -2)
                        && distance_column_previous_move = 0
                      then
                        let attack_good_coord_for_en_passant =
                          if get_color_from_player next_player = Black then
                            ( previous_move_coord_final_line - 1,
                              previous_move_coord_final_column )
                            = current_move_coord_final
                          else
                            ( previous_move_coord_final_line + 1,
                              previous_move_coord_final_column )
                            = current_move_coord_final
                        in
                        if attack_good_coord_for_en_passant then
                          let b =
                            delete_piece b
                              ( previous_move_coord_final_line,
                                previous_move_coord_final_column )
                          in
                          Some
                            (move_from_coord_to_coord b current_move_coord_start
                               current_move_coord_final)
                        else
                          let () = Format.printf "1@;" in
                          None
                      else
                        let () = Format.printf "2@;" in
                        None
                  | _ ->
                      let () = Format.printf "3@;" in
                      None)
              | _ ->
                  let () = Format.printf "4@;" in
                  None)
          | _ ->
              let () = Format.printf "5@;" in
              None)
      | _ ->
          let () = Format.printf "6@;" in
          None)
  | _ ->
      let () = Format.printf "7@;" in
      None

(*c is the colour of the person being attacked*)
let attacked_coord_by_enemy (b : board) (coord : coordinates) (c : color) =
  if is_valide_coordinates coord then
    let rec aux i j =
      if i > 7 then false
      else if j > 7 then aux (i + 1) 0
      else
        match get_piece b (i, j) with
        | Some piece ->
            if c <> piece.color && can_move b piece (i, j) coord then true
            else aux i (j + 1)
        | None -> aux i (j + 1)
    in
    aux 0 0
  else raise Invalide_coordinates

(*Check if the player c is losing or not*)
let chess (b : board) (c : color) : bool option =
  let rec aux line column =
    match get_piece b (line, column) with
    | Some { shape = King _; color } when color = c -> Some (line, column)
    | _ ->
        let column = column + 1 in
        if column > 7 then
          let line = line + 1 in
          if line > 7 then None else aux line 0
        else aux line column
  in
  let king_coord = aux 0 0 in
  match king_coord with
  | Some (line, column) -> Some (attacked_coord_by_enemy b (line, column) c)
  | None -> None

(*Check if player c has lost*)
let chess_mate (b : board) (c : color) =
  let rec aux line column =
    match get_piece b (line, column) with
    | Some { shape = King _; color } when color = c -> Some (line, column)
    | _ ->
        let column = column + 1 in
        if column > 7 then
          let line = line + 1 in
          if line > 7 then None else aux line 0
        else aux line column
  in
  let king_coord = aux 0 0 in
  match king_coord with
  | Some (line, column) ->
      Some
        (attacked_coord_by_enemy b (line, column) c
        && attacked_coord_by_enemy b (line + 1, column) c
        && attacked_coord_by_enemy b (line + 1, column + 1) c
        && attacked_coord_by_enemy b (line, column + 1) c
        && attacked_coord_by_enemy b (line - 1, column + 1) c
        && attacked_coord_by_enemy b (line - 1, column) c
        && attacked_coord_by_enemy b (line - 1, column - 1) c
        && attacked_coord_by_enemy b (line, column - 1) c
        && attacked_coord_by_enemy b (line + 1, column - 1) c)
  | None -> None

let play_move (b : board) (current_player : player) (m : move)
    (next_player : player) =
  let current_player_color = get_color_from_player current_player in
  match m with
  | Movement (coord_start, coord_final) ->
      if is_valide_coordinates coord_start && is_valide_coordinates coord_final
      then
        match get_piece b coord_start with
        | None -> None
        | Some piece ->
            if
              piece.color = current_player_color
              &&
              match get_piece b coord_final with
              | None -> true
              | Some piece_coord_final ->
                  piece_coord_final.color <> current_player_color
            then
              if can_move b piece coord_start coord_final then
                let new_b =
                  move_from_coord_to_coord b coord_start coord_final
                in
                if
                  let are_chess = chess new_b current_player_color in
                  are_chess = Some true || are_chess = None
                then None
                else Some new_b
              else
                let new_b = try_passant b piece m next_player in
                match new_b with
                | Some new_b ->
                    if
                      let are_chess = chess new_b current_player_color in
                      are_chess = Some true || are_chess = None
                    then None
                    else Some new_b
                | None -> None
            else None
      else raise Invalide_coordinates
  | Big_Castling -> (
      let coord_king =
        if current_player_color = Black then (0, 4) else (7, 4)
      in
      let coord_rook =
        if current_player_color = Black then (0, 0) else (7, 0)
      in
      match (get_piece b coord_king, get_piece b coord_king) with
      | Some { shape = King b1; color = _ }, Some { shape = Rook b2; color = _ }
        ->
          if b1 && b2 && empty_straight b coord_king coord_rook then
            let new_coord_king =
              if current_player_color = Black then (0, 2) else (7, 2)
            in
            let new_coord_rook =
              if current_player_color = Black then (0, 3) else (7, 3)
            in
            let new_b = move_from_coord_to_coord b coord_king new_coord_king in
            let new_b =
              move_from_coord_to_coord new_b coord_rook new_coord_rook
            in
            if
              let are_chess = chess new_b current_player_color in
              are_chess = Some true || are_chess = None
            then None
            else Some new_b
          else None
      | _, _ -> None)
  | Small_Castling -> (
      let coord_king =
        if current_player_color = Black then (0, 4) else (7, 4)
      in
      let coord_rook =
        if current_player_color = Black then (0, 7) else (7, 7)
      in
      match (get_piece b coord_king, get_piece b coord_king) with
      | Some { shape = King b1; color = _ }, Some { shape = Rook b2; color = _ }
        ->
          if b1 && b2 && empty_straight b coord_king coord_rook then
            let new_coord_king =
              if current_player_color = Black then (0, 6) else (7, 6)
            in
            let new_coord_rook =
              if current_player_color = Black then (0, 5) else (7, 5)
            in
            let new_b = move_from_coord_to_coord b coord_king new_coord_king in
            let new_b =
              move_from_coord_to_coord new_b coord_rook new_coord_rook
            in
            if
              let are_chess = chess new_b current_player_color in
              are_chess = Some true || are_chess = None
            then None
            else Some new_b
          else None
      | _, _ -> None)

let get_value_of_board b = Array.copy b
