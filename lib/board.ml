open Piece

type board = piece option list list
type coordonne = int * int

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
        | 3 -> Some (if color = Some White then Queen else King)
        | 4 -> Some (if color = Some White then King else Queen)
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
