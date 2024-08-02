open Piece
open Global

type board = {
  board : piece option list list;
  last_move : move option;
  dead_piece : piece option list;
  current_player : color;
  fifty_moves : int;
}

type board_status =
  | Continue_board of board
  | Draw_board of board
  | Error_board of string

let get_piece (b : board) (c : coordinates) =
  if is_valid_coordinates c then
    match c with x, y -> List.nth (List.nth b.board x) y
  else raise Invalid_coordinates

let char_to_piece = function
  | 'K' -> Some { shape = King false; color = White }
  | 'Q' -> Some { shape = Queen; color = White }
  | 'R' -> Some { shape = Rook false; color = White }
  | 'B' -> Some { shape = Bishop; color = White }
  | 'N' -> Some { shape = Knight; color = White }
  | 'P' -> Some { shape = Pawn false; color = White }
  | 'k' -> Some { shape = King false; color = Black }
  | 'q' -> Some { shape = Queen; color = Black }
  | 'r' -> Some { shape = Rook false; color = Black }
  | 'b' -> Some { shape = Bishop; color = Black }
  | 'n' -> Some { shape = Knight; color = Black }
  | 'p' -> Some { shape = Pawn false; color = Black }
  | _ -> None

let rec fold_left_i f i acc l =
  match l with [] -> acc | x :: xs -> fold_left_i f (i + 1) (f i x acc) xs

exception Invalid_fen

let parse (current_line : piece option list) = function
  | '1' .. '8' as num ->
      let num_spaces = int_of_char num - int_of_char '0' in
      let rec add_empty i l =
        if i = 0 then l else None :: add_empty (i - 1) l
      in
      add_empty num_spaces current_line
  | char -> char_to_piece char :: current_line

let generate_board_with_fen (fen : string) : board =
  let fen = String.split_on_char ' ' fen in
  match fen with
  | [
   info_piece;
   current_player;
   castling;
   en_passant;
   fifty_moves;
   (*number_of_move*) _;
  ] -> (
      let info_piece = String.split_on_char '/' info_piece in
      let board =
        List.fold_left
          (fun acc1 s ->
            String.fold_left (fun acc2 c -> parse acc2 c) [] s :: acc1)
          [] info_piece
      in
      let current_player =
        match current_player with
        | "w" -> White
        | "b" -> Black
        | _ -> raise Invalid_fen
      in
      let board =
        fold_left_i
          (fun x line acc1 ->
            fold_left_i
              (fun y piece acc2 ->
                (match piece with
                | Some { shape = King false; color = Black } ->
                    if
                      String.contains castling 'k'
                      || String.contains castling 'q'
                    then
                      if (x, y) = (7, 3) then
                        Some { shape = King true; color = Black }
                      else raise Invalid_fen
                    else Some { shape = King false; color = Black }
                | Some { shape = Rook false; color = Black } ->
                    if
                      (String.contains castling 'k'
                      || String.contains castling 'q')
                      && (x, y) <> (7, 0)
                      && (x, y) <> (7, 7)
                    then raise Invalid_fen
                    else if (x, y) = (7, 0) then
                      if String.contains castling 'k' then
                        Some { shape = Rook true; color = Black }
                      else Some { shape = Rook false; color = Black }
                    else if (x, y) = (7, 7) then
                      if String.contains castling 'q' then
                        Some { shape = Rook true; color = Black }
                      else Some { shape = Rook false; color = Black }
                    else Some { shape = Rook false; color = Black }
                | Some { shape = King false; color = White } ->
                    if
                      String.contains castling 'K'
                      || String.contains castling 'Q'
                    then
                      if (x, y) = (0, 3) then
                        Some { shape = King true; color = White }
                      else raise Invalid_fen
                    else Some { shape = King false; color = White }
                | Some { shape = Rook false; color = White } ->
                    if
                      (String.contains castling 'k'
                      || String.contains castling 'q')
                      && (x, y) <> (0, 0)
                      && (x, y) <> (0, 7)
                    then raise Invalid_fen
                    else if (x, y) = (0, 0) then
                      if String.contains castling 'k' then
                        Some { shape = Rook true; color = White }
                      else Some { shape = Rook false; color = White }
                    else if (x, y) = (0, 7) then
                      if String.contains castling 'q' then
                        Some { shape = Rook true; color = White }
                      else Some { shape = Rook false; color = White }
                    else Some { shape = Rook false; color = White }
                | Some { shape = Pawn false; color = White } ->
                    if x = 1 then Some { shape = Pawn true; color = White }
                    else Some { shape = Pawn false; color = White }
                | Some { shape = Pawn false; color = Black } ->
                    if x = 6 then Some { shape = Pawn true; color = Black }
                    else Some { shape = Pawn false; color = Black }
                | piece -> piece)
                :: acc2)
              0 [] line
            :: acc1)
          0 [] board
      in
      if
        List.length board <> 8
        || List.exists (fun l -> List.length l <> 8) board
      then raise Invalid_fen
      else
        let cmp_white_king =
          List.fold_left
            (fun acc1 line ->
              List.fold_left
                (fun acc2 piece ->
                  match piece with
                  | Some { shape = King _; color = White } -> acc2 + 1
                  | _ -> acc2)
                acc1 line)
            0 board
        in
        let cmp_black_king =
          List.fold_left
            (fun acc1 line ->
              List.fold_left
                (fun acc2 piece ->
                  match piece with
                  | Some { shape = King _; color = Black } -> acc2 + 1
                  | _ -> acc2)
                acc1 line)
            0 board
        in
        if cmp_white_king != 1 || cmp_black_king != 1 then raise Invalid_fen
        else
          let b =
            {
              board;
              last_move = None;
              dead_piece = [];
              current_player;
              fifty_moves =
                (try
                   let fifty_moves = int_of_string fifty_moves in
                   if 0 <= fifty_moves && fifty_moves < 50 then fifty_moves
                   else raise Invalid_fen
                 with _ -> raise Invalid_fen);
            }
          in
          match en_passant with
          | "-" -> { b with last_move = None }
          | coord ->
              let l, c = convert_coordinates coord in
              if current_player = Black then
                if
                  get_piece b (l + 1, c) <> None
                  || get_piece b (l, c) <> None
                  || get_piece b (l - 1, c)
                     <> Some { shape = Pawn false; color = White }
                then raise Invalid_fen
                else
                  {
                    b with
                    last_move = Some (Movement ((l + 1, c), (l - 1, c)));
                  }
              else if
                get_piece b (l - 1, c) <> None
                || get_piece b (l, c) <> None
                || get_piece b (l + 1, c)
                   <> Some { shape = Pawn false; color = Black }
              then raise Invalid_fen
              else
                { b with last_move = Some (Movement ((l - 1, c), (l + 1, c))) })
  | _ -> raise Invalid_fen

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
let get_last_move_from_board b = b.last_move
let get_current_player_from_board b = b.current_player

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
  else raise Invalid_coordinates

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
  else raise Invalid_coordinates

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
        | Knight ->
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
  else raise Invalid_coordinates

(*Removes the piece from the starting coordinate.
   Puts the deleted piece on the new coordinate*)
let move_from_coord_to_coord (b : board) (coord_start : coordinates)
    (coord_final : coordinates) =
  assert (is_valid_coordinates coord_start && is_valid_coordinates coord_final);
  let piece = get_piece b coord_start in
  let b = set_piece b coord_start None in
  Option.map
    (fun piece ->
      match piece.shape with
      | King _ -> { piece with shape = King false }
      | Rook _ -> { piece with shape = Rook false }
      | Pawn _ -> { piece with shape = Pawn false }
      | _ -> piece)
    piece
  |> set_piece b coord_final

(*p is the piece we want to move*)
let can_en_passant (b : board) (m : move) =
  match (m, b.last_move) with
  | ( Movement
        ( (current_move_coord_start_line, current_move_coord_start_column),
          (current_move_coord_final_line, current_move_coord_final_column) ),
      Some
        (Movement
          ( (previous_move_coord_start_line, previous_move_coord_start_column),
            (previous_move_coord_final_line, previous_move_coord_final_column)
          )) ) -> (
      assert (
        is_valid_coordinates
          (current_move_coord_start_line, current_move_coord_start_column)
        && is_valid_coordinates
             (current_move_coord_final_line, current_move_coord_final_column));
      match
        ( get_piece b
            (current_move_coord_start_line, current_move_coord_start_column),
          get_piece b
            (previous_move_coord_final_line, previous_move_coord_final_column)
        )
      with
      | ( Some { shape = Pawn _; color = current_piece_color },
          Some { shape = Pawn _; color = enemy_piece_color } ) ->
          let is_an_attack = current_piece_color <> enemy_piece_color in
          let current_piece_advance_of_one_in_diag =
            let distance_line_current_move =
              current_move_coord_start_line - current_move_coord_final_line
            in
            let distance_column_current_move =
              current_move_coord_start_column - current_move_coord_final_column
            in
            ((distance_line_current_move = -1 && current_piece_color = Black)
            || (distance_line_current_move = 1 && current_piece_color = White))
            && (distance_column_current_move = 1
               || distance_column_current_move = -1)
          in
          let enemy_pawn_advance_of_two =
            let distance_line_previous_move =
              previous_move_coord_start_line - previous_move_coord_final_line
            in
            let distance_column_previous_move =
              previous_move_coord_start_column
              - previous_move_coord_final_column
            in
            (distance_line_previous_move = 2 || distance_line_previous_move = -2)
            && distance_column_previous_move = 0
          in
          let attack_good_coord_for_en_passant =
            if enemy_piece_color = Black then
              ( previous_move_coord_final_line - 1,
                previous_move_coord_final_column )
              = (current_move_coord_final_line, current_move_coord_final_column)
            else
              ( previous_move_coord_final_line + 1,
                previous_move_coord_final_column )
              = (current_move_coord_final_line, current_move_coord_final_column)
          in
          current_piece_advance_of_one_in_diag && is_an_attack
          && enemy_pawn_advance_of_two && attack_good_coord_for_en_passant
      | _ -> false)
  | _ -> false

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
  else raise Invalid_coordinates

exception No_King

let find_king (b : board) (c : color) : coordinates =
  let rec aux line column =
    match get_piece b (line, column) with
    | Some { shape = King _; color } when color = c -> (line, column)
    | _ ->
        let column = column + 1 in
        if column > 7 then
          let line = line + 1 in
          if line > 7 then raise No_King else aux line 0
        else aux line column
  in
  aux 0 0

(*Check if the player c is losing or not*)
let chess (b : board) (c : color) : bool =
  attacked_coord_by_enemy b (find_king b c) c

let need_promotion b (coord_final_line, coord_final_column) =
  match get_piece b (coord_final_line, coord_final_column) with
  | Some { shape = Pawn _; color = c } ->
      (coord_final_line = 0 && c = White) || (coord_final_line = 7 && c = Black)
  | _ -> false

let play_move (b : board) current_player_choose_promotion_strategy (m : move) =
  let current_player_color = b.current_player in
  let b =
    {
      b with
      current_player = get_other_color b.current_player;
      fifty_moves =
        (if current_player_color = Black then b.fifty_moves + 1
         else b.fifty_moves + 1);
    }
  in
  match m with
  | Movement (coord_start, coord_final) ->
      let b =
        let fifty_rule =
          (match get_piece b coord_start with
          | Some { shape = Pawn _; color = _ } -> true
          | _ -> false)
          ||
          match get_piece b coord_final with
          | Some p -> p.color <> current_player_color
          | None -> false
        in
        { b with fifty_moves = (if fifty_rule then 0 else b.fifty_moves) }
      in
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
                if chess b current_player_color then None
                else
                  let b =
                    if need_promotion b coord_final then
                      set_piece b coord_final
                        (Some
                           {
                             shape =
                               (match
                                  current_player_choose_promotion_strategy b
                                with
                               | Rook true -> Rook false
                               | s -> s);
                             color = current_player_color;
                           })
                    else b
                  in
                  let b =
                    if enemy_on_destination then
                      {
                        b with
                        last_move = Some m;
                        dead_piece = dead_piece :: b.dead_piece;
                      }
                    else { b with last_move = Some m }
                  in
                  if b.fifty_moves >= 50 then Some (Draw_board b)
                  else Some (Continue_board b)
              else
                match (b.last_move, can_en_passant b m) with
                | Some (Movement (_, previous_move_coord_final)), true ->
                    let dead_piece = get_piece b previous_move_coord_final in
                    let b = set_piece b previous_move_coord_final None in
                    let b =
                      move_from_coord_to_coord b coord_start coord_final
                    in
                    if chess b current_player_color then None
                    else
                      let b =
                        {
                          b with
                          last_move = Some m;
                          dead_piece = dead_piece :: b.dead_piece;
                        }
                      in
                      if b.fifty_moves >= 50 then Some (Draw_board b)
                      else Some (Continue_board b)
                | _, _ -> None
            else None
      else raise Invalid_coordinates
  | Big_Castling | Small_Castling -> (
      let coord_king =
        if current_player_color = Black then (0, 4) else (7, 4)
      in
      let coord_rook =
        if m = Big_Castling then
          if current_player_color = Black then (0, 0) else (7, 0)
        else if current_player_color = Black then (0, 7)
        else (7, 7)
      in
      match (get_piece b coord_king, get_piece b coord_rook) with
      | ( Some { shape = King b1; color = color1 },
          Some { shape = Rook b2; color = color2 } ) ->
          if
            b1 && b2
            && empty_straight b coord_king coord_rook
            && color1 = current_player_color
            && color2 = current_player_color
          then
            let new_coord_king =
              if m = Big_Castling then
                if current_player_color = Black then (0, 2) else (7, 2)
              else if current_player_color = Black then (0, 6)
              else (7, 6)
            in
            let new_coord_rook =
              if m = Big_Castling then
                if current_player_color = Black then (0, 3) else (7, 3)
              else if current_player_color = Black then (0, 5)
              else (7, 5)
            in
            let b = move_from_coord_to_coord b coord_king new_coord_king in
            let b = move_from_coord_to_coord b coord_rook new_coord_rook in
            if chess b current_player_color then None
            else
              let b = { b with last_move = Some m } in
              if b.fifty_moves >= 50 then Some (Draw_board b)
              else Some (Continue_board b)
          else None
      | _, _ -> None)
  | _ -> None

let get_possible_move (piece : piece) ((l, c) : coordinates) =
  let get_line_column_coord (i : int) =
    if i < 8 then if i < c then (l, i) else (l, i + 1)
    else
      let i = i - 8 in
      if i < l then (i, c) else (i + 1, c)
  in
  let get_diag_coord (i : int) =
    if i < 8 then (l - i - 1, c - i - 1)
    else
      let i = i - 8 in
      if i < 8 then (l + i + 1, c - i - 1)
      else
        let i = i - 8 in
        if i < 8 then (l - i + 1, c - i - 1)
        else
          let i = i - 8 in
          (l - i - 1, c + i + 1)
  in
  List.filter
    (fun c -> is_valid_coordinates c)
    (match (piece.shape, piece.color) with
    | Rook _, _ -> List.init 14 (fun i -> get_line_column_coord i)
    | Knight, _ ->
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
    | Bishop, _ -> List.init 32 (fun i -> get_diag_coord i)
    | Pawn _, Black -> [ (l + 1, c + 1); (l + 1, c - 1); (l + 1, c) ]
    | Pawn _, White -> [ (l - 1, c + 1); (l - 1, c - 1); (l - 1, c) ]
    | King _, _ ->
        [
          (l + 1, c + 1);
          (l - 1, c + 1);
          (l - 1, c - 1);
          (l + 1, c - 1);
          (l + 1, c);
          (l, c + 1);
          (l - 1, c);
          (l, c - 1);
        ]
    | Queen, _ ->
        List.init 46 (fun i ->
            if i < 15 then get_line_column_coord i else get_diag_coord (15 - i)))

(*Check if player c has lost*)
let chess_mate (b : board) (c : color) : bool =
  let try_move (line_start, column_start) (line_finish, column_finish) =
    try
      play_move b
        (fun _ -> Queen)
        (Movement ((line_start, column_start), (line_finish, column_finish)))
      <> None
    with Invalid_coordinates -> false
  in
  chess b c
  && not
       (fold_left_i
          (fun (n_line : int) (line : piece option list) (acc1 : bool) ->
            acc1
            || fold_left_i
                 (fun (n_column : int) (piece : piece option) (acc2 : bool) ->
                   match piece with
                   | None -> acc2
                   | Some piece ->
                       if piece.color <> c then acc2
                       else
                         acc2
                         || List.exists
                              (fun (x, y) -> try_move (n_line, n_column) (x, y))
                              (get_possible_move piece (n_line, n_column)))
                 0 false line)
          0 false (get_board_from_board b))

let adjacent_possibles_move (piece : piece) ((l, c) : coordinates) =
  List.filter
    (fun c -> is_valid_coordinates c)
    (match piece.shape with
    | Rook _ -> [ (l + 1, c); (l, c + 1); (l - 1, c); (l, c - 1) ]
    | Knight ->
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

let stalemate (b : board) =
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
                  (*If a piece is to be promoted in this test. It will be promoted as queen. It does not change anything*)
                  play_move b (fun _ -> Queen) (Movement ((l, c), coord))
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
