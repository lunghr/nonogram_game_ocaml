open Drawer_lib
open Image_generator
open Graphics
open Math

let rec run_lvl board lvl lives lvl_num =
  match lives with
  | 0 ->
      lose ();
      new_game lvl_num
  | _ -> (
      match check_win board lvl with
      | true -> (
          match int_of_string lvl_num with
          | l when l <= 1 -> (
              match next_level lvl with
              | true -> new_game (string_of_int (l + 1))
              | false -> ())
          | _ -> win lvl)
      | false ->
          let updated_board, updated_lives = handle_click board lvl lives in
          run_lvl updated_board lvl updated_lives lvl_num)

and new_game lvl_num =
  let lvl = upload_lvl lvl_num in
  let board = generate_blank_board lvl.board_size in
  Graphics.set_window_title lvl.lvl_name;
  clear_graph ();
  print_numbers lvl.rows lvl.cols;
  draw_hearts board;
  draw_board board;
  draw_grid lvl.board_size;
  run_lvl board lvl 3 lvl_num
