open Global

type shape =
  | King of bool
  | Queen
  | Rook of bool
  | Bishop
  | Horse
  | Pawn of bool

type piece = { shape : shape; color : color }

let pp_piece fmt piece =
  let p_str =
    match piece.shape with
    | King _ -> ( match piece.color with Black -> "♚" | White -> "♔")
    | Queen -> ( match piece.color with Black -> "♛" | White -> "♕")
    | Rook _ -> ( match piece.color with Black -> "♜" | White -> "♖")
    | Bishop -> ( match piece.color with Black -> "♝" | White -> "♗")
    | Horse -> ( match piece.color with Black -> "♞" | White -> "♘")
    | Pawn _ -> ( match piece.color with Black -> "♟" | White -> "♙")
  in
  Format.fprintf fmt "%s" p_str
