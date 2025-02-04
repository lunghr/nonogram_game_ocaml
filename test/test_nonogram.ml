open Image_generator

let test_board =
  [
    [ Filled; Empty; Filled; Empty; Filled ];
    [ Empty; Filled; Empty; Filled; Empty ];
    [ Filled; Empty; Filled; Empty; Filled ];
    [ Empty; Filled; Empty; Filled; Filled ];
    [ Filled; Empty; Filled; Empty; Filled ];
  ]

let test_columns = [ [ 1; 1; 1 ]; [ 1; 1 ]; [ 1; 1; 1 ]; [ 1; 1 ]; [ 3; 1 ] ]

let test_count_filled_cells_in_every_column () =
  let columns = calculate_filled_cells_in_every_column test_board in
  Alcotest.(check (list (list int))) "same columns" test_columns columns

let suite =
  [
    ( "test_count_filled_cells_in_every_column",
      `Quick,
      test_count_filled_cells_in_every_column );
  ]

let () = Alcotest.run "test_nonogram" [ ("test_nonogram", suite) ]
