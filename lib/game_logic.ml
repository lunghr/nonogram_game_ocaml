open Graphics
open Drawer_lib
open Image_generator

let rec run_lvl board lvl lives =
  match lives with
  | 0 ->
      clear_graph ();
      moveto (size_x () / 2) (size_y () / 2);
      set_color black;
      draw_string "You lose!";
      let _ = wait_next_event [ Button_down ] in
      new_game lvl
  | _ ->
      let updated_board, updated_lives = handle_click board lvl lives in
      run_lvl updated_board lvl updated_lives

and new_game lvl =
  let board = generate_blank_board lvl.board_size in
  Graphics.set_window_title lvl.lvl_name;
  print_numbers lvl.rows lvl.cols;
  draw_hearts board;
  draw_board board;
  draw_grid lvl.board_size;
  run_lvl board lvl 3
