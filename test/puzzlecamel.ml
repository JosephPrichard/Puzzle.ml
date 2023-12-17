(* Tests for 8puzzle *)
let () =
  let tiles = Puzzle.tiles_of_str "8 6 7 2 0 4 3 5 1" in
  let number = Puzzle.tiles_of_number tiles 0 in
  Printf.printf "Number %d\n" number;
  let visited = Puzzle.VisitedSet.add number () Puzzle.VisitedSet.empty in
  let has = Puzzle.VisitedSet.find_opt (Puzzle.tiles_of_number tiles 0) visited in
  let _ = match has with
  | Some _ -> Printf.printf "Yes it is contained\n"
  | None -> Printf.printf "No it is not contained\n"
  in 
  let tiles = Puzzle.tiles_of_str "0 8 7 2 6 5 3 1 4" in
  let h = Puzzle.calc_heurstic tiles in
  Printf.printf "Heuristic: %d\n\n" h;
  let _ = Puzzle.next_tiles tiles (0, 0) (Puzzle.next_positions (0, 0)) [] in
  ()
