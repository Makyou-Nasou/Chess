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
        | 0 -> Some Rook
        | 1 -> Some Horse
        | 2 -> Some Bishop
        | 3 -> Some (if color = Some White then Queen else King true)
        | 4 -> Some (if color = Some White then King true else Queen)
        | 5 -> Some Bishop
        | 6 -> Some Horse
        | 7 -> Some Rook
        | _ -> None
      in
      match (color, shape) with
      | None, _ | _, None -> None
      | Some c, Some s -> Some { shape = s; color = c })

let init_board () =
  List.init 8 (fun line ->
      List.init 8 (fun column -> get_starter_piece (line, column)))

let pp_board fmt board =
  let () = Format.fprintf fmt "―――――――――――――――――――――――――――――――――@." in
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
      else Format.fprintf fmt "|@.―――――――――――――――――――――――――――――――――@."
    in
    aux_print_line 0
  in
  let rec print_board (i : int) =
    if i <= 7 then (
      print_line i;
      print_board (i + 1))
    else ()
  in
  print_board 0

let get_piece (b : board) (c : coordonne) =
  match c with x, y -> List.nth (List.nth b x) y

let set_element_list list indice new_element =
  let rec aux_set_list list current_indice =
    match list with
    | a :: b ->
        if current_indice = indice then new_element :: b
        else a :: aux_set_list b (current_indice + 1)
    | [] -> []
  in
  aux_set_list list 0

(*Exchange the pieces of the two indicated boxes*)
let switch_coord (b : board) (coord_start : coordonne) (coord_final : coordonne)
    =
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
          set_element_list final_line final_column_number piece
        in
        let b = set_element_list b final_line_number final_line in
        set_element_list b start_line_number start_line

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
      let distance_line = coord_start_line - coord_final_line in
      let distance_column = coord_start_column - coord_final_column in
      match p.shape with
      | King _ ->
          distance_column = 1 || distance_column = -1 || distance_line = 1
          || distance_line = -1
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
      | Rook ->
          ((distance_column = 0 && distance_line <> 0)
          || (distance_line = 0 && distance_line <> 0))
          && empty_straight b coord_start coord_final
      | Horse ->
          (distance_line = 2 || distance_line = -2)
          && (distance_column = 1 || distance_column = -1)
          || (distance_line = 1 || distance_line = -1)
             && (distance_column = 2 || distance_column = -2)
      | Pawn first_move ->
          (distance_line = 0
           && ((dir_column = 2 && p.color = White)
              || (dir_column = -2 && p.color = Black))
           && first_move
          || (dir_column = 1 && p.color = White)
          || (dir_column = -1 && p.color = Black))
          || (distance_line = 1 || distance_line = -1)
             && ((dir_column = 1 && p.color = White)
                || (dir_column = -1 && p.color = Black))
             && get_piece coord_final <> None)

let move (b : board) (p : player) (m : move) =
  match m with
  | Movement (coord_start, coord_final) -> (
      match get_piece b coord_start with
      | None -> None
      | Some piece ->
          if
            piece.color = get_color_from_player p
            &&
            match get_piece b coord_final with
            | None -> true
            | Some piece_coord_final ->
                piece_coord_final.color <> get_color_from_player p
          then
            if can_move b piece coord_start coord_final then
              Some (switch_coord b coord_start coord_final)
            else None
          else None)
  | _ -> Some (switch_coord b (0, 0) (0, 0))
