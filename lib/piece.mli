type shape = King | Queen | Rook | Bishop | Horse | Pawn

type color = Black | White

type piece = {shape : shape; color : color}

let pp_piece fmt piece color=
    let p_str =
        match piece with
        | King -> (match color with
          | Black -> "♚"
          | White -> "♔")
        | Queen -> (match color with
          | Black -> "♛"
          | White -> "♕")
        | Rook -> (match color with
          | Black -> "♜"
          | White -> "♖")
        | Bishop -> (match color with
          | Black -> "♝"
          | White -> "♗")
        | Horse -> (match color with
          | Black -> "♞"
          | White -> "♘")
        | Pawn -> (match color with
          | Black -> "♟"
          | White -> "♙")
    in Format.fprintf fmt "%s" p_str