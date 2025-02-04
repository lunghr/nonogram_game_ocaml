open Yojson.Safe.Util
open Yojson.Safe

type cell = Filled | Empty | Unknown

type lvl = {
  lvl_name : string;
  board_size : int;
  board : cell list list;
  rows : int list list;
  cols : int list list;
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
  (*  Printf.printf "lvl_name: %s\n" lvl_name; *)
  let board_size = json |> member "board_size" |> to_int in
  (*  Printf.printf "board_size: %d\n" board_size; *)
  let board = json |> member "board" |> board_of_json in
  (*  Printf.printf "board: %d\n" (List.length board); *)
  let rows = calculate_filled_cells_in_every_row board in
  (*  List.iter (fun row -> List.iter (Printf.printf "%d ") row; Printf.printf "\n") rows; *)
  let cols = calculate_filled_cells_in_every_column board in
  (*  List.iter *)
  (*    (fun col -> *)
  (*      List.iter (Printf.printf "%d ") col; *)
  (*      Printf.printf "\n") *)
  (*    cols; *)
  { lvl_name; board_size; board; rows; cols }

let upload_lvl lvl_name =
  let filename = "levels/" ^ lvl_name ^ ".json" in
  (*  Printf.printf "file with name %s loaded\n" filename; *)
  let json = from_file filename in
  lvl_of_json json

let check_row_filling game_board lvl =
  let game_board_rows = calculate_filled_cells_in_every_row game_board in
  List.mapi (fun i row -> row = List.nth lvl.rows i) game_board_rows

let check_column_filling game_board lvl =
  let game_board_cols = calculate_filled_cells_in_every_column game_board in
  List.mapi (fun i col -> col = List.nth lvl.cols i) game_board_cols
