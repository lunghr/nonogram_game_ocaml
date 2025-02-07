open Image_generator
open Graphics

let cell_size = 60

let count_offsets size cell_size =
  let board_size = size * cell_size in
  let offset_x = (size_x () - board_size) / 2 in
  let offset_y = ((size_y () - board_size) / 2) - 40 in
  (offset_x, offset_y)

let calculate_cell_coordinates x y size =
  let offset_x, offset_y = count_offsets size cell_size in
  let x = offset_x + (x * cell_size) in
  let y = offset_y + (y * cell_size) in
  (x, y)

let check_hitting x y size =
  let offset_x, offset_y = count_offsets size cell_size in
  x >= offset_x
  && x <= offset_x + (size * cell_size)
  && y >= offset_y
  && y <= offset_y + (size * cell_size)

let update_board game_board target_cell x y =
  List.mapi
    (fun i row ->
      match i with
      | i when i = y ->
          List.mapi
            (fun j cell ->
              match j with j when j = x -> target_cell | _ -> cell)
            row
      | _ -> row)
    game_board

let count_heart_offsets size =
  let _, offset_y = count_offsets size cell_size in
  let x = (size_x () - 320) / 2 in
  let y = size_y () - ((size_y () - (offset_y + (size * cell_size))) / 2) in
  (x, y)

let check_requirements condition board filled_list index =
  List.exists (fun cell -> cell = Unknown) (condition board index)
  && List.nth filled_list index

let compare_rows row1 row2 =
  let rec aux acc = function
    | [], [] -> acc
    | x :: xs, y :: ys -> aux (acc && x = y) (xs, ys)
    | _ -> false
  in
  aux true (row1, row2)

let find_cell i row =
  let rec aux j = function
    | [] -> Unknown
    | x :: xs ->
        if j = i then
          x
        else
          aux (j + 1) xs
  in
  aux 0 row

let check_win game_board lvl =
  List.for_all2 (fun row1 row2 -> compare_rows row1 row2) game_board lvl.board
