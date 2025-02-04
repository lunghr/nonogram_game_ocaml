open Graphics
open Game_logic
open Image_generator

let () =
  let _ = open_graph " 1200x1200" in
  let lvl = upload_lvl "1" in
  new_game lvl
