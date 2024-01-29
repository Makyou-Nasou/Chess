open Piece
open Global
open Player

type board = {
  board : piece option list list;
  last_move : move option;
  dead_piece : piece option list;
}

let get_starter_piece ((line, column) : coordinates) =
  assert (is_valid_coordinates (line, column));
  match line with
  | 1 -> Some { shape = Pawn true; color = Black }
  | 6 -> Some { shape = Pawn true; color = White }
  | _ -> (
      let color =
        match line with 0 -> Some Black | 7 -> Some White | _ -> None
      in
      let shape =
        match column with
        | 0 | 7 -> Some (Rook true)
        | 1 | 6 -> Some Horse
        | 2 | 5 -> Some Bishop
        | 3 -> Some Queen
        | 4 -> Some (King true)
        | _ -> None
      in
      match (color, shape) with
      | None, _ | _, None -> None
      | Some c, Some s -> Some { shape = s; color = c })

let init_board () =
  {
    board =
      List.init 8 (fun line ->
          List.init 8 (fun column -> get_starter_piece (line, column)));
    last_move = None;
    dead_piece = [];
  }

let pp_board fmt b =
  let () =
    Format.fprintf fmt
      " | a | b | c | d | e | f | g | h@.――――――――――――――――――――――――――――――――――@."
  in
  let print_line line_number =
    let rec aux_print_line column_number =
      if column_number <= 7 then (
        Format.fprintf fmt "| ";
        let p = List.nth (List.nth b.board line_number) column_number in
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
  let () = print_board 0 in
  let rec print_dead_piece l =
    match l with
    | Some p :: t ->
        pp_piece fmt p;
        Format.fprintf fmt " ";
        print_dead_piece t
    | None :: t ->
        Format.fprintf fmt "? ";
        print_dead_piece t
    | [] -> ()
  in
  if List.length b.dead_piece <> 0 then
    let () = Format.fprintf fmt "Dead piece :@ " in
    let () = print_dead_piece b.dead_piece in
    Format.fprintf fmt "@ "
  else ()

let get_board_from_board b = b.board

let get_piece (b : board) (c : coordinates) =
  if is_valid_coordinates c then
    match c with x, y -> List.nth (List.nth b.board x) y
  else
    let () = Format.printf "1@ " in
    raise Invalid_coordinates

let set_piece (b : board) (line, column) piece =
  {
    b with
    board =
      List.mapi
        (fun i current_line ->
          if i = line then
            List.mapi
              (fun j current_column ->
                if j = column then piece else current_column)
              current_line
          else current_line)
        b.board;
  }

(*We don’t check whether the starting box or the arriving box are empty.
   We just look at whether the intermediate boxes are empty.*)
let empty_straight (b : board) (coord_start : coordinates)
    (coord_final : coordinates) =
  if is_valid_coordinates coord_start && is_valid_coordinates coord_final then
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
  else
    let () = Format.printf "2@ " in
    raise Invalid_coordinates

(*We don’t check whether the starting box or the arriving box are empty.
   We just look at whether the intermediate boxes are empty.*)
let empty_diagonal (b : board) (coord_start : coordinates)
    (coord_final : coordinates) =
  if is_valid_coordinates coord_start && is_valid_coordinates coord_final then
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
  else
    let () = Format.printf "3@ " in
    raise Invalid_coordinates

(*We assume that the arrival box is empty or occupied by an enemy.
   That the starting box is occupied by the piece given as an argument.*)
let can_move (b : board) (p : piece) (coord_start : coordinates)
    (coord_final : coordinates) =
  if is_valid_coordinates coord_start && is_valid_coordinates coord_final then
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
  else
    let () = Format.printf "4@ " in
    raise Invalid_coordinates

(*Removes the piece from the starting coordinate.
   Puts the deleted piece on the new coordinate*)
let move_from_coord_to_coord (b : board) (coord_start : coordinates)
    (coord_final : coordinates) =
  assert (is_valid_coordinates coord_start && is_valid_coordinates coord_final);
  let piece = get_piece b coord_start in
  let b = set_piece b coord_start None in
  set_piece b coord_final
    (match piece with
    | None -> None
    | Some piece -> (
        match piece.shape with
        | King _ -> Some { piece with shape = King false }
        | Rook _ -> Some { piece with shape = Rook false }
        | Pawn _ -> Some { piece with shape = Pawn false }
        | _ -> Some piece))

(*p is the piece we want to move*)
let try_passant (b : board) (p : piece) (m : move) (next_player : player) =
  match m with
  | Movement (current_move_coord_start, current_move_coord_final) -> (
      assert (
        is_valid_coordinates current_move_coord_start
        && is_valid_coordinates current_move_coord_final);
      match p.shape with
      | Pawn _ -> (
          match b.last_move with
          | Some
              (Movement
                ( ( previous_move_coord_start_line,
                    previous_move_coord_start_column ),
                  ( previous_move_coord_final_line,
                    previous_move_coord_final_column ) )) -> (
              assert (
                is_valid_coordinates
                  ( previous_move_coord_start_line,
                    previous_move_coord_start_column )
                && is_valid_coordinates
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
                          let dead_piece =
                            get_piece b
                              ( previous_move_coord_final_line,
                                previous_move_coord_final_column )
                          in
                          let b =
                            set_piece b
                              ( previous_move_coord_final_line,
                                previous_move_coord_final_column )
                              None
                          in
                          let b =
                            move_from_coord_to_coord b current_move_coord_start
                              current_move_coord_final
                          in
                          Some
                            { b with dead_piece = dead_piece :: b.dead_piece }
                        else None
                      else None
                  | _ -> None)
              | _ -> None)
          | _ -> None)
      | _ -> None)
  | _ -> None

(*c is the color of the person being attacked*)
let attacked_coord_by_enemy (b : board) (coord : coordinates) (c : color) =
  if is_valid_coordinates coord then
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
  else
    let () = Format.printf "5@ " in
    raise Invalid_coordinates

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
        (try attacked_coord_by_enemy b (line, column) c
         with Invalid_coordinates -> (
           true
           &&
           try attacked_coord_by_enemy b (line + 1, column) c
           with Invalid_coordinates -> (
             true
             &&
             try attacked_coord_by_enemy b (line + 1, column + 1) c
             with Invalid_coordinates -> (
               true
               &&
               try attacked_coord_by_enemy b (line, column + 1) c
               with Invalid_coordinates -> (
                 true
                 &&
                 try attacked_coord_by_enemy b (line - 1, column + 1) c
                 with Invalid_coordinates -> (
                   true
                   &&
                   try attacked_coord_by_enemy b (line - 1, column) c
                   with Invalid_coordinates -> (
                     true
                     &&
                     try attacked_coord_by_enemy b (line - 1, column - 1) c
                     with Invalid_coordinates -> (
                       true
                       &&
                       try attacked_coord_by_enemy b (line, column - 1) c
                       with Invalid_coordinates -> (
                         true
                         &&
                         try attacked_coord_by_enemy b (line + 1, column - 1) c
                         with Invalid_coordinates -> true)))))))))
  | None -> None

let need_promotion b current_player (coord_final_line, coord_final_column) =
  match get_piece b (coord_final_line, coord_final_column) with
  | Some { shape = Pawn _; color = c } ->
      get_color_from_player current_player = c
      && ((coord_final_line = 0 && c = White)
         || (coord_final_line = 7 && c = Black))
  | _ -> false

let play_move (b : board) (current_player : player) (m : move)
    (next_player : player) =
  let current_player_color = get_color_from_player current_player in
  match m with
  | Movement (coord_start, coord_final) ->
      if is_valid_coordinates coord_start && is_valid_coordinates coord_final
      then
        match get_piece b coord_start with
        | None -> None
        | Some piece ->
            let dead_piece = get_piece b coord_final in
            let empty_destination, enemy_on_destination =
              match dead_piece with
              | None -> (true, false)
              | Some piece_coord_final ->
                  (false, piece_coord_final.color <> current_player_color)
            in
            if
              piece.color = current_player_color
              && (empty_destination || enemy_on_destination)
            then
              if can_move b piece coord_start coord_final then
                let b = move_from_coord_to_coord b coord_start coord_final in
                if
                  let are_chess = chess b current_player_color in
                  are_chess = Some true || are_chess = None
                then None
                else if need_promotion b current_player coord_final then
                  let b =
                    set_piece b coord_final
                      (Some
                         {
                           shape = (get_choose_promotion current_player) b.board;
                           color = current_player_color;
                         })
                  in
                  if enemy_on_destination then
                    Some
                      {
                        b with
                        last_move = Some m;
                        dead_piece = dead_piece :: b.dead_piece;
                      }
                  else Some { b with last_move = Some m }
                else if enemy_on_destination then
                  Some
                    {
                      b with
                      last_move = Some m;
                      dead_piece = dead_piece :: b.dead_piece;
                    }
                else Some { b with last_move = Some m }
              else
                match try_passant b piece m next_player with
                | Some b ->
                    if
                      let are_chess = chess b current_player_color in
                      are_chess = Some true || are_chess = None
                    then None
                    else Some { b with last_move = Some m }
                | None -> None
            else None
      else
        let () = Format.printf "6@ " in
        raise Invalid_coordinates
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
            let b = move_from_coord_to_coord b coord_king new_coord_king in
            let b = move_from_coord_to_coord b coord_rook new_coord_rook in
            if
              let are_chess = chess b current_player_color in
              are_chess = Some true || are_chess = None
            then None
            else Some { b with last_move = Some Big_Castling }
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
            let b = move_from_coord_to_coord b coord_king new_coord_king in
            let b = move_from_coord_to_coord b coord_rook new_coord_rook in
            if
              let are_chess = chess b current_player_color in
              are_chess = Some true || are_chess = None
            then None
            else Some { b with last_move = Some Small_Castling }
          else None
      | _, _ -> None)
  | _ -> None

let adjacent_possibles_move (piece : piece) ((l, c) : coordinates) =
  List.filter
    (fun c -> is_valid_coordinates c)
    (match piece.shape with
    | Rook _ -> [ (l + 1, c); (l, c + 1); (l - 1, c); (l, c - 1) ]
    | Horse ->
        [
          (l + 1, c + 2);
          (l + 2, c + 1);
          (l + 1, c - 2);
          (l - 2, c + 1);
          (l - 1, c + 2);
          (l + 2, c - 1);
          (l - 2, c - 1);
          (l - 1, c - 2);
        ]
    | Bishop ->
        [ (l + 1, c + 1); (l - 1, c + 1); (l - 1, c - 1); (l + 1, c - 1) ]
    | Pawn _ | Queen | King _ ->
        [
          (l + 1, c + 1);
          (l - 1, c + 1);
          (l - 1, c - 1);
          (l + 1, c - 1);
          (l + 1, c);
          (l, c + 1);
          (l - 1, c);
          (l, c - 1);
        ])

let stalemate (b : board) current_player next_player =
  let rec aux l c =
    if l > 7 then true
    else if c > 7 then aux (l + 1) 0
    else
      let current_piece = get_piece b (l, c) in
      match current_piece with
      | Some current_piece ->
          if
            List.exists
              (fun coord ->
                match
                  play_move b current_player
                    (Movement ((l, c), coord))
                    next_player
                with
                | Some _ -> false
                | None -> true)
              (adjacent_possibles_move current_piece (l, c))
          then false
          else aux l (c + 1)
      | None -> aux l (c + 1)
  in
  aux 0 0

let equals_boards (b1 : board) (b2 : piece option list list) = b1.board = b2
