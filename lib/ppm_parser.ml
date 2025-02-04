open Graphics

let load_ppm filename =
  let ic = open_in_bin filename in
  let magic = input_line ic in
  if magic <> "P6" then failwith "Unsupported format (expected P6)";

  let dims = input_line ic in
  let width, height =
    match String.split_on_char ' ' (String.trim dims) with
    | [ w; h ] -> (int_of_string w, int_of_string h)
    | _ -> failwith "Invalid PPM dimensions"
  in

  let _ = input_line ic in

  let pixels = Array.make_matrix height width 0 in

  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let r = input_byte ic in
      let g = input_byte ic in
      let b = input_byte ic in
      pixels.(y).(x) <- rgb r g b
    done
  done;

  close_in ic;
  pixels

let scale_image image new_width new_height =
  let old_width = Array.length image.(0) in
  let old_height = Array.length image in
  let scaled_pixels = Array.make_matrix new_height new_width 0 in

  for y = 0 to new_height - 1 do
    for x = 0 to new_width - 1 do
      let old_x = x * old_width / new_width in
      let old_y = y * old_height / new_height in
      scaled_pixels.(y).(x) <- image.(old_y).(old_x)
    done
  done;

  make_image scaled_pixels
