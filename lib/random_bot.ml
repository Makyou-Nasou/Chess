open Player
open Piece
open Global
open Board

let cartesian_product list1 list2 =
  let rec aux1 l1 =
    match l1 with
    | [] -> []
    | a :: b ->
        let rec aux2 l2 =
          match l2 with [] -> [] | c :: d -> (a, c) :: (c, a) :: aux2 d
        in
        aux2 list2 @ aux1 b
  in
  aux1 list1

let all_posible_move () =
  let res = List.init 8 (fun i -> i) in
  let res = cartesian_product res res in
  List.map (fun (c1, c2) -> Movement (c1, c2)) (cartesian_product res res)

let random_choose_promotion _ _ =
  List.nth
    [ Queen; Rook (List.nth [ true; false ] (Random.int 2)); Horse; Bishop ]
    (Random.int 4)

let random_choose_move (color : color) (board : board) =
  let res =
    List.filter
      (fun x -> Board.play_move board (random_choose_promotion color) x <> None)
      (all_posible_move ())
  in
  let res = Small_Castling :: Big_Castling :: res in
  List.nth res (Random.int (List.length res))

let random_choose_accept_draw _ _ = List.nth [ true; false ] (Random.int 2)

let random_strategy () =
  {
    choose_move = random_choose_move;
    choose_accept_draw = random_choose_accept_draw;
    choose_promotion = random_choose_promotion;
  }
