open Graphics
open Ppm_parser
open Image_generator
open Math

let draw_grid size =
  set_color (rgb 178 183 200);
  let offset_x, offset_y = count_offsets size cell_size in
  for i = 0 to size do
    let x = offset_x + (i * cell_size) in
    moveto x offset_y;
    lineto x (offset_y + (size * cell_size))
  done;

  for i = 0 to size do
    let y = offset_y + (i * cell_size) in
    moveto offset_x y;
    lineto (offset_x + (size * cell_size)) y
  done

let draw_cell x y cell size =
  let x, y = calculate_cell_coordinates x y size in
  match cell with
  | Filled ->
      set_color (rgb 39 44 60);
      fill_rect x y cell_size cell_size
  | Empty ->
      set_color white;
      fill_rect x y cell_size cell_size;
      draw_image
        (scale_image (load_ppm "graphic/cross.ppm") cell_size cell_size)
        x y
  | Unknown ->
      set_color white;
      fill_rect x y cell_size cell_size

let draw_board board =
  let size = List.length board in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      draw_cell j i (List.nth (List.nth board i) j) size
    done
  done

let print_numbers rows cols =
  let size = List.length rows in
  let offset_x, offset_y = count_offsets size cell_size in
  let rec print_row_numbers row row_index i =
    match row with
    | [] -> ()
    | x :: xs ->
        moveto
          (offset_x - (cell_size / 2 * i) - 20)
          (offset_y + ((cell_size * row_index) + 20));
        set_color (rgb 39 44 60);
        draw_string (string_of_int x);
        print_row_numbers xs row_index (i + 1)
  in
  let rec print_col_numbers col col_index i =
    let col_size = List.length col in
    match col with
    | [] -> ()
    | x :: xs ->
        moveto
          (offset_x + (cell_size * col_index) + 30)
          (offset_y + (size * cell_size) + (cell_size / 2 * col_size) - 16);
        set_color (rgb 39 44 60);
        draw_string (string_of_int x);
        print_col_numbers xs col_index (i + 1)
  in
  for i = 0 to size - 1 do
    print_row_numbers (List.nth rows i) i 0;
    print_col_numbers (List.nth cols i) i 0
  done

let generate_blank_board size =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (acc @ [ List.init size (fun _ -> Unknown) ]) (n - 1)
  in
  aux [] size

let draw_hearts board =
  let heart = scale_image (load_ppm "graphic/heart.ppm") 100 100 in
  let x, y = count_heart_offsets (List.length board) in

  let rec aux x y = function
    | 0 -> ()
    | n ->
        draw_image heart x y;
        aux (x + 110) y (n - 1)
  in
  aux x y 3

let dec_heart board available_hearts =
  let heart = scale_image (load_ppm "graphic/grey_heart.ppm") 100 100 in
  let x, y = count_heart_offsets (List.length board) in
  draw_image heart (x + (110 * available_hearts)) y

let print_cross j i size board lvl_board =
  let cell = List.nth (List.nth lvl_board i) j in
  draw_cell j i cell size;
  draw_grid size;
  update_board board cell j i

let update_crosses game_board lvl get_cells check_filling swap_coords =
  let filled = check_filling game_board lvl in
  let size = List.length game_board in
  List.fold_left
    (fun acc_board (i, _) ->
      if check_requirements get_cells acc_board filled i then
        List.fold_left
          (fun acc_board (j, _) ->
            let x, y = swap_coords i j in
            print_cross x y size acc_board lvl.board)
          acc_board
          (List.mapi (fun j cell -> (j, cell)) (get_cells lvl.board i))
      else
        acc_board)
    game_board
    (List.mapi (fun i row -> (i, row)) lvl.board)

let print_row_crosses game_board lvl =
  update_crosses game_board lvl
    (fun board i -> List.nth board i)
    check_row_filling
    (fun i j -> (j, i))

let print_column_crosses game_board lvl =
  update_crosses game_board lvl
    (fun board i -> List.map (fun row -> List.nth row i) board)
    check_column_filling
    (fun i j -> (i, j))

let handle_click game_board lvl lives =
  let solution_board = lvl.board in
  let status = wait_next_event [ Button_down ] in
  let offset_x, offset_y = count_offsets (List.length game_board) cell_size in
  let mx, my = (status.mouse_x, status.mouse_y) in

  match check_hitting mx my (List.length game_board) with
  | false -> (game_board, lives)
  | true -> (
      let x = (mx - offset_x) / cell_size in
      let y = (my - offset_y) / cell_size in
      let cell = find_cell x (List.nth game_board y) in

      match cell with
      | Unknown -> (
          let solution_cell = find_cell x (List.nth solution_board y) in
          match solution_cell with
          | Filled ->
              draw_cell x y Filled (List.length game_board);
              update_board game_board Filled x y
              |> fun board ->
              print_row_crosses board lvl
              |> fun board ->
              print_column_crosses board lvl
              |> fun board ->
              draw_grid (List.length board);
              (board, lives)
          | Empty ->
              draw_cell x y Empty (List.length game_board);
              draw_grid (List.length game_board);
              dec_heart game_board (lives - 1);
              (update_board game_board Empty x y, lives - 1)
          | Unknown -> (game_board, lives))
      | _ -> (game_board, lives))

let colour_board lvl =
  let size = List.length lvl.board in
  List.iteri
    (fun i row ->
      List.iteri
        (fun j _ ->
          let x, y = calculate_cell_coordinates j i size in
          let r, g, b = List.nth (List.nth lvl.colors i) j in
          set_color (rgb r g b);
          fill_rect x y cell_size cell_size)
        row)
    lvl.board

let display_next_level_button () =
  let img = scale_image (load_ppm "graphic/next_level.ppm") 200 100 in
  draw_image img (size_x () - 300) 70

let next_level lvl =
  colour_board lvl;
  display_next_level_button ();
  let status = wait_next_event [ Button_down ] in
  let x, y = (status.mouse_x, status.mouse_y) in
  x >= size_x () - 300 && x <= size_x () - 100 && y >= 70 && y <= 170

let load_end_screen img_name =
  clear_graph ();
  moveto (size_x () / 2) (size_y () / 2);
  let img = scale_image (load_ppm ("graphic/" ^ img_name ^ ".ppm")) 600 252 in
  draw_image img ((size_x () - 600) / 2) ((size_y () - 252) / 2)

let lose () =
  load_end_screen "lose";
  let _ = wait_next_event [ Button_down ] in
  clear_graph ()

let win lvl =
  colour_board lvl;
  let _ = wait_next_event [ Button_down ] in
  load_end_screen "win";
  let _ = wait_next_event [ Button_down ] in
  exit 0
