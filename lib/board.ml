open Piece
open Player
open Global

type board = piece option list list
type move = Movement of coordonne * coordonne | Small_Castling | Big_Castling

let get_starter_piece ((line, column) : coordonne) =
  match line with
  | 1 -> Some { shape = Pawn; color = Black }
  | 6 -> Some { shape = Pawn; color = White }
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

let set_list list indice new_element =
  let rec aux_set_list list current_indice =
    match list with
    | a :: b ->
        if current_indice = indice then new_element :: b
        else a :: aux_set_list b (current_indice + 1)
    | [] -> []
  in
  aux_set_list list 0

let switch_coord (b : board) (coord_start : coordonne) (coord_final : coordonne)
    =
  match (coord_start, coord_final) with
  | ( (start_line_number, start_column_number),
      (final_line_number, final_column_number) ) ->
      let piece = get_piece b (start_line_number, start_column_number) in
      let start_line = List.nth b start_line_number in
      let start_line = set_list start_line start_column_number None in
      if start_line_number = final_line_number then
        let final_line = start_line in
        let final_line = set_list final_line final_column_number piece in
        set_list b final_line_number final_line
      else
        let final_line = List.nth b final_line_number in
        let final_line = set_list final_line final_column_number piece in
        let b = set_list b final_line_number final_line in
        set_list b start_line_number start_line

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
      | _ -> true || b = b)

let classic_move (b : board) (p : player) (m : move) =
  match m with
  | Movement (coord_start, coord_final) -> (
      match get_piece b coord_start with
      | None -> None
      | Some piece ->
          if
            piece.color = p.color
            &&
            match get_piece b coord_final with
            | None -> true
            | Some piece_coord_final -> piece_coord_final.color <> p.color
          then
            if can_move b piece coord_start coord_final then
              Some (switch_coord b coord_start coord_final)
            else None
          else None)
  | _ -> Some (switch_coord b (0, 0) (0, 0))
