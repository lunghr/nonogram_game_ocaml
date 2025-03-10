open Yojson.Safe.Util
open Yojson.Safe

type cell = Filled | Empty | Unknown
type board = cell list list

type lvl = {
  lvl_name : string;
  board_size : int;
  board : cell list list;
  rows : int list list;
  cols : int list list;
  colors : (int * int * int) list list;
}

let cell_of_string s =
  match s with "\"Filled\"" -> Filled | "\"Empty\"" -> Empty | _ -> Unknown

let board_of_json json =
  let board =
    json
    |> to_list
    |> List.map (fun row ->
           row
           |> to_list
           |> List.map (fun cell -> cell_of_string (to_string cell)))
  in
  List.rev board

let parse_colors json =
  json
  |> to_list
  |> List.map (fun row ->
         row
         |> to_list
         |> List.map (fun color ->
                match to_list color with
                | [ r; g; b ] -> (to_int r, to_int g, to_int b)
                | _ -> failwith "Неверный формат цвета"))
  |> List.rev

let calculate_filled_cells_in_every_column board =
  let size = List.length board in
  let columns =
    List.init size (fun i -> List.map (fun row -> List.nth row i) board)
  in

  let count_filled_cells column =
    let rec aux acc count = function
      | [] ->
          if count > 0 then
            count :: acc
          else
            acc
      | Filled :: tail -> aux acc (count + 1) tail
      | _ :: tail ->
          if count = 0 then
            aux acc 0 tail
          else
            aux (count :: acc) 0 tail
    in
    aux [] 0 column
  in

  List.map count_filled_cells columns

let calculate_filled_cells_in_every_row board =
  List.map
    (fun row ->
      let rec aux acc count = function
        | [] ->
            if count > 0 then
              count :: acc
            else
              acc
        | Filled :: tail -> aux acc (count + 1) tail
        | _ :: tail ->
            if count = 0 then
              aux acc 0 tail
            else
              aux (count :: acc) 0 tail
      in
      aux [] 0 row)
    board

let lvl_of_json json =
  let lvl_name = json |> member "name" |> to_string in
  let board_size = json |> member "board_size" |> to_int in
  let board = json |> member "board" |> board_of_json in
  let rows = calculate_filled_cells_in_every_row board in
  let cols = calculate_filled_cells_in_every_column board in
  let colors = json |> member "colors" |> parse_colors in
  { lvl_name; board_size; board; rows; cols; colors }

let upload_lvl lvl_name =
  let filename = "levels/" ^ lvl_name ^ ".json" in
  let json = from_file filename in
  lvl_of_json json

let check_row_filling game_board lvl =
  let game_board_rows = calculate_filled_cells_in_every_row game_board in
  List.mapi (fun i row -> row = List.nth lvl.rows i) game_board_rows

let check_column_filling game_board lvl =
  let game_board_cols = calculate_filled_cells_in_every_column game_board in
  List.mapi (fun i col -> col = List.nth lvl.cols i) game_board_cols
