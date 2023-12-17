(* Solving the 8puzzle *)
let () =
  let file = "8puzzle.txt" in
  let tiles = Puzzle.tiles_of_chan (open_in file) in
  let solution = Puzzle.solve tiles in
  Printf.printf "-Solution-\n\n";
  let () = Puzzle.print_solution solution in
  ()