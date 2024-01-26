open Piece
open Player
open Global

type board = piece option list list

let get_starter_piece ((line, column) : coordonne) =
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

let get_line_from_board (b : board) i = List.nth b i

let init_board () =
  List.init 8 (fun line ->
      List.init 8 (fun column -> get_starter_piece (line, column)))

let pp_board fmt board =
  let () =
    Format.fprintf fmt
      " | a | b | c | d | e | f | g | h@.――――――――――――――――――――――――――――――――――@."
  in
  let print_line line_number =
    let rec aux_print_line column_number =
      if column_number <= 7 then (
        Format.fprintf fmt "| ";
        let p = List.nth (List.nth board line_number) column_number in
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

let get_piece (b : board) (c : coordonne) =
  match c with x, y -> List.nth (List.nth b x) y

(*We don’t check whether the starting box or the arriving box are empty.
   We just look at whether the intermediate boxes are empty.*)
let empty_straight (b : board) (coord_start : coordonne)
    (coord_final : coordonne) =
  match (coord_start, coord_final) with
  | ( (coord_start_line, coord_start_column),
      (coord_final_line, coord_final_column) ) ->
      if
        coord_start_line < 0 || coord_start_line > 7 || coord_final_line < 0
        || coord_final_column > 7
      then false
      else
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
            if coord_start = coord_final then true
            else
              get_piece b (coord_current_line, coord_current_line_colum) = None
              && aux
                   ( coord_current_line + dir_line,
                     coord_current_line_colum + dir_column )
          in
          aux (coord_start_line + dir_line, coord_start_column + dir_column)

(*We don’t check whether the starting box or the arriving box are empty.
   We just look at whether the intermediate boxes are empty.*)
let empty_diagonal (b : board) (coord_start : coordonne)
    (coord_final : coordonne) =
  match (coord_start, coord_final) with
  | ( (coord_start_line, coord_start_column),
      (coord_final_line, coord_final_column) ) ->
      if
        coord_start_line < 0 || coord_start_line > 7 || coord_final_line < 0
        || coord_final_column > 7
      then false
      else
        let distance_line = coord_start_line - coord_final_line in
        let distance_column = coord_start_column - coord_final_column in
        if distance_column <> distance_line then false
        else
          let dir_line = if distance_line < 0 then 1 else -1 in
          let dir_column = if distance_column < 0 then 1 else -1 in
          let rec aux (coord_current_line, coord_current_line_colum) =
            if coord_start = coord_final then true
            else
              get_piece b (coord_current_line, coord_current_line_colum) = None
              && aux
                   ( coord_current_line + dir_line,
                     coord_current_line_colum + dir_column )
          in
          aux (coord_start_line + dir_line, coord_start_column + dir_column)

(*We assume that the arrival box is empty or occupied by an enemy.
   That the starting box is occupied by the piece given as an argument.*)
let can_move (b : board) (p : piece) (coord_start : coordonne)
    (coord_final : coordonne) =
  match (coord_start, coord_final) with
  | ( (coord_start_line, coord_start_column),
      (coord_final_line, coord_final_column) ) -> (
      if
        coord_start_line < 0 || coord_start_line > 7 || coord_start_column < 0
        || coord_start_column > 7 || coord_final_line < 0
        || coord_final_line > 7 || coord_final_column < 0
        || coord_final_column > 7
      then false
      else
        let distance_line = coord_start_line - coord_final_line in
        let distance_column = coord_start_column - coord_final_column in
        match p.shape with
        | King _ ->
            (distance_column = 1 || distance_column = -1 || distance_line = 1
           || distance_line = -1)
            && distance_column <= 1 && distance_column >= -1
            && distance_line <= 1 && distance_line >= -1
        | Queen ->
            (distance_column = distance_line
            || distance_column = -distance_line
               && empty_diagonal b coord_start coord_final)
            || ((distance_column = 0 && distance_line <> 0)
               || (distance_line = 0 && distance_line <> 0))
               && empty_straight b coord_start coord_final
        | Bishop ->
            distance_column = distance_line
            || distance_column = -distance_line
               && empty_diagonal b coord_start coord_final
        | Rook _ ->
            ((distance_column = 0 && distance_line <> 0)
            || (distance_line = 0 && distance_line <> 0))
            && empty_straight b coord_start coord_final
        | Horse ->
            (distance_line = 2 || distance_line = -2)
            && (distance_column = 1 || distance_column = -1)
            || (distance_line = 1 || distance_line = -1)
               && (distance_column = 2 || distance_column = -2)
        | Pawn first_move ->
            (distance_column = 0
             && ((distance_line = 2 && p.color = White)
                || (distance_line = -2 && p.color = Black))
             && first_move
            || (distance_line = 1 && p.color = White)
            || (distance_line = -1 && p.color = Black))
            || (distance_column = 1 || distance_column = -1)
               && ((distance_line = 1 && p.color = White)
                  || (distance_line = -1 && p.color = Black))
               && get_piece b coord_final <> None)

let set_element_list list indice new_element =
  let rec aux_set_list list current_indice =
    match list with
    | a :: b ->
        if current_indice = indice then new_element :: b
        else a :: aux_set_list b (current_indice + 1)
    | [] -> []
  in
  aux_set_list list 0

(*Removes the part from the starting coordinate. Puts the deleted part on the new coordinate*)
let move_from_coord_to_coord (b : board) (coord_start : coordonne)
    (coord_final : coordonne) =
  match (coord_start, coord_final) with
  | ( (start_line_number, start_column_number),
      (final_line_number, final_column_number) ) ->
      let piece = get_piece b (start_line_number, start_column_number) in
      let start_line = List.nth b start_line_number in
      let start_line = set_element_list start_line start_column_number None in
      if start_line_number = final_line_number then
        let final_line = start_line in
        let final_line =
          set_element_list final_line final_column_number piece
        in
        set_element_list b final_line_number final_line
      else
        let final_line = List.nth b final_line_number in
        let final_line =
          set_element_list final_line final_column_number
            (match piece with
            | None -> None
            | Some piece -> (
                match piece.shape with
                | King _ -> Some { piece with shape = King false }
                | Rook _ -> Some { piece with shape = Rook false }
                | Pawn _ -> Some { piece with shape = Pawn false }
                | _ -> Some piece))
        in
        let b = set_element_list b final_line_number final_line in
        set_element_list b start_line_number start_line

let delete_piece (b : board) ((cl, cc) : coordonne) =
  let new_line = set_element_list (List.nth b cl) cc None in
  set_element_list b cl new_line

let try_passant (b : board) (p : piece) (next_player : player) (m : move) =
  match m with
  | Movement (current_move_coord_start, current_move_coord_final) -> (
      match p.shape with
      | Pawn _ -> (
          match get_last_move_from_player next_player with
          | Some
              (Movement
                ( ( previou_move_coord_start_line,
                    previou_move_coord_start_column ),
                  previou_move_coord_final )) -> (
              match get_piece b previou_move_coord_final with
              | Some piece -> (
                  match piece.shape with
                  | Pawn _ ->
                      let coord_to_attack =
                        if get_color_from_player next_player = Black then
                          ( previou_move_coord_start_line + 1,
                            previou_move_coord_start_column )
                        else
                          ( previou_move_coord_start_line - 1,
                            previou_move_coord_start_column )
                      in
                      if current_move_coord_final = coord_to_attack then
                        let b = delete_piece b previou_move_coord_final in
                        Some
                          (move_from_coord_to_coord b current_move_coord_start
                             current_move_coord_final)
                      else None
                  | _ -> None)
              | _ -> None)
          | _ -> None)
      | _ -> None)
  | _ -> None

let attacked_coord_by_enemy (b : board) (coord : coordonne) (c : color) =
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

let chess (b : board) (c : color) : bool =
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

let move (b : board) (current_player : player) (next_player : player) (m : move)
    =
  match m with
  | Movement (coord_start, coord_final) -> (
      match get_piece b coord_start with
      | None -> None
      | Some piece ->
          if
            piece.color = get_color_from_player current_player
            &&
            match get_piece b coord_final with
            | None -> true
            | Some piece_coord_final ->
                piece_coord_final.color <> get_color_from_player current_player
          then
            if can_move b piece coord_start coord_final then
              let new_b = move_from_coord_to_coord b coord_start coord_final in
              if chess new_b (get_color_from_player current_player) then None
              else Some new_b
            else
              let new_b = try_passant b piece next_player m in
              match new_b with
              | Some new_b ->
                  if chess new_b (get_color_from_player current_player) then
                    None
                  else Some new_b
              | None -> None
          else None)
  | Big_Castling -> (
      let coord_king =
        if get_color_from_player current_player = Black then (0, 4) else (7, 4)
      in
      let coord_rook =
        if get_color_from_player current_player = Black then (0, 0) else (7, 0)
      in
      match (get_piece b coord_king, get_piece b coord_king) with
      | Some { shape = King b1; color = _ }, Some { shape = Rook b2; color = _ }
        ->
          if b1 && b2 && empty_straight b coord_king coord_rook then
            let new_coord_king =
              if get_color_from_player current_player = Black then (0, 2)
              else (7, 2)
            in
            let new_coord_rook =
              if get_color_from_player current_player = Black then (0, 3)
              else (7, 3)
            in
            let new_b = move_from_coord_to_coord b coord_king new_coord_king in
            let new_b =
              move_from_coord_to_coord new_b coord_rook new_coord_rook
            in
            if chess new_b (get_color_from_player current_player) then None
            else Some new_b
          else None
      | _, _ -> None)
  | Small_Castling -> (
      let coord_king =
        if get_color_from_player current_player = Black then (0, 4) else (7, 4)
      in
      let coord_rook =
        if get_color_from_player current_player = Black then (0, 7) else (7, 7)
      in
      match (get_piece b coord_king, get_piece b coord_king) with
      | Some { shape = King b1; color = _ }, Some { shape = Rook b2; color = _ }
        ->
          if b1 && b2 && empty_straight b coord_king coord_rook then
            let new_coord_king =
              if get_color_from_player current_player = Black then (0, 6)
              else (7, 6)
            in
            let new_coord_rook =
              if get_color_from_player current_player = Black then (0, 5)
              else (7, 5)
            in
            let new_b = move_from_coord_to_coord b coord_king new_coord_king in
            let new_b =
              move_from_coord_to_coord new_b coord_rook new_coord_rook
            in
            if chess new_b (get_color_from_player current_player) then None
            else Some new_b
          else None
      | _, _ -> None)
