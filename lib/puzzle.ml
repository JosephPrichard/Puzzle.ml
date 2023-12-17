type tile = Empty | One | Two | Three | Four | Five | Six | Seven | Eight
type puzzle = { 
  parent: puzzle option;
  tiles: tile array;
  gscore: int; 
  fscore: int 
}
type position = (int * int)

let size = 3 (* NxN size of the puzzle *)
let goal_tiles = [|Empty; One; Two; Three; Four; Five; Six; Seven; Eight;|]

let pos_of_index index = (index / size, index mod size)

let index_of_pos (row, col) = row * size + col

let int_of_tile = function
  | Empty -> 0
  | One -> 1
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8

let tile_of_int int = match int with
  | 0 -> Empty
  | 1 -> One
  | 2 -> Two
  | 3 -> Three
  | 4 -> Four
  | 5 -> Five
  | 6 -> Six
  | 7 -> Seven
  | 8 -> Eight
  | _ -> failwith (Printf.sprintf "Invalid integer %d for tile" int)

let tile_of_string str =
  try tile_of_int (int_of_string str)
  with _ -> failwith (Printf.sprintf "Invalid str %s of tile" str) 

let string_of_tile tile = string_of_int (int_of_tile tile)

let in_bounds pos = let (row, col) = pos in row >= 0 && row < size && col >= 0 && col < size

(* Gets the tile at a position on the tiles, raises an exception if the position is invalid *)
let get_pos tiles pos =
  let i = index_of_pos pos in
  if i < Array.length tiles && i >= 0 then
    tiles.(i)
  else 
    raise (Invalid_argument 
      (Printf.sprintf "Cannot get index %i - is an invalid position" i))

(* New tiles with the position at tiles set to the tile - raises an exception if the position is invalid *)
let set_pos tiles pos tile =
  let tiles = Array.copy tiles in
  let i = index_of_pos pos in
  if i < Array.length tiles && i >= 0 then
    let () = tiles.(i) <- tile in
    tiles
  else
    raise (Invalid_argument 
      (Printf.sprintf "Cannot get index %i - is an invalid position" i))

(* Search for empty tile on the tiles and return the position - raise exception if the empty tile doesn't exist - should NOT happen*)  
let rec find_empty tiles i =
  if i < Array.length tiles then
    (if tiles.(i) == Empty then pos_of_index i
    else
      find_empty tiles (i+1))
  else
    raise (Invalid_argument "Puzzle doesn't have an empty tile")

(* Get the next positions conforming to a pattern relative to a position on the matrix - only containing positions within the boundary *)
let next_positions pos = 
  let (row, col) = pos in
  let direction_to_pos direction = 
    let (r, c) = direction in 
    (col + r, row + c) 
  in
  List.filter in_bounds 
    (List.map direction_to_pos 
      [(1, 0); (0, 1); (-1, 0); (0, -1)])

(* Swap two tiles on the tiles for the given positions *)
let swap tiles pos_i pos_j =
  (set_pos 
    (set_pos tiles pos_i (get_pos tiles pos_j)) 
    pos_j 
    (get_pos tiles pos_i))

(* Gets the swapped tiles for each next position relative to the provided position into an accumulator *)
let rec next_tiles tiles provided_pos next_positions acc = 
  match next_positions with 
  | next_pos :: next_positions ->
    next_tiles tiles provided_pos next_positions 
      (swap tiles provided_pos next_pos :: acc)
  | [] -> acc

let manhattan_distance pos1 pos2 =
  let (row1, col1) = pos1 in
  let (row2, col2) = pos2 in
  abs (row2 - row1) + abs (col2 - col1)

(* Calculate the heuristic of the given tiles relative to a PREDEFINED GOAL STATE *)
let calc_heurstic tiles =
  let reduce_heuristic acc tile = 
    let (sum_h, i) = acc in
    (* Position of the tile on tiles *)
    let tile_pos = pos_of_index i in 
    (* Position of the tile on goal_tiles - same value as the tile itself *)
    let goal_pos = pos_of_index (int_of_tile tile) in
    let md = manhattan_distance tile_pos goal_pos in
    (sum_h + md, i + 1) 
  in
  let (h, _) = Array.fold_left reduce_heuristic (0, 0) tiles in h 

let tiles_of_str str = 
  let split = Str.split (Str.regexp "[ \n\r\x0c\t]+") str in
  Array.of_list (List.map tile_of_string split)

let tiles_of_chan ic =
  let rec str_of_ch ic = 
    try
      let line = input_line ic in
      line ^ "\n" ^ (str_of_ch ic)
    with
    | End_of_file -> ""
    | e ->
      close_in_noerr ic;
      raise e
  in
  let str = str_of_ch ic in
  tiles_of_str str

let rec tiles_of_number tiles i = 
  let exp = int_of_float (10. ** float_of_int i) in
  if i < Array.length tiles then
    int_of_tile tiles.(i) * exp + tiles_of_number tiles (i+1)
  else 
    0

(* Get the next puzzles for a given puzzle - each generated next puzzle will be linked to this puzzle as a child*)
let next_puzzles puzzle =
  let empty_pos = find_empty puzzle.tiles 0 in
  let tiles_acc = next_tiles puzzle.tiles empty_pos (next_positions empty_pos) [] in
  (* Makes a new puzzle for each of the tiles in the tiles accumulator *)
  let make_puzzle tiles =
    let gscore = puzzle.gscore + 1 in
    let fscore = gscore + calc_heurstic tiles in
    {parent = Some puzzle; tiles = tiles; gscore = gscore; fscore = fscore} 
  in
  List.map make_puzzle tiles_acc

let print_puzzle puzzle =
  Printf.printf "Puzzle\nG: %d, F: %d\n" puzzle.gscore puzzle.fscore;
  let print_tile i tile =
    let term = if (i+1) mod 3 == 0 then "\n" else " " in
    Printf.printf "%s" (string_of_tile tile ^ term);
    () 
  in
  let () = Array.iteri print_tile puzzle.tiles in
  Printf.printf "\n";

(* The ordered puzzles uses a key that sorts puzzles by heuristics *)
module OrderedPuzzles = Map.Make(struct
  type t = puzzle
  let compare puzzle1 puzzle2 = puzzle1.fscore - puzzle2.fscore
end) 

(* Visited set to check if numbers exist in a set *)
module VisitedSet = Map.Make(struct
  type t = int
  let compare num1 num2 = num1 - num2
end)

let rec reconstruct_path puzzle path =
  let path = puzzle :: path in
  match puzzle.parent with 
  | Some parent -> reconstruct_path parent path
  | None -> List.rev path

let always _ = true

let rec search frontier visited bound =
  if bound <= 0 then [] else
  (* Get the BEST puzzle from the frontier - no puzzle means we end the search with no solution *)
  match OrderedPuzzles.find_first_opt always frontier with
  | Some (puzzle, _) ->
    print_puzzle puzzle;
    let frontier = OrderedPuzzles.remove puzzle frontier in
    (* A puzzle must be removed from the frontier and added to visited set - we don't want to search it again *)
    let visited = VisitedSet.add (tiles_of_number puzzle.tiles 0) () visited in
    (* Check if the puzzle matches the goal solution *)
    if puzzle.tiles = goal_tiles then
      reconstruct_path puzzle []
    else
      let rec add_neighbors puzzles frontier = 
        (* Try to add the neighbor to frontier but only if it is not visited *)
        match puzzles with
        | puzzle :: puzzles -> 
          (* A neighbor can only be added into the frontier if it has not already been visited*)
          (match VisitedSet.find_opt (tiles_of_number puzzle.tiles 0) visited with 
          | Some () -> add_neighbors puzzles frontier
          | None -> add_neighbors puzzles (OrderedPuzzles.add puzzle () frontier))
        | [] -> frontier
      in
      search (add_neighbors (next_puzzles puzzle) frontier) visited (bound-1)
  | None -> []
    
let solve tiles =
  let root = {parent = None; tiles = tiles; gscore = 0; fscore = 0} in
  let frontier = OrderedPuzzles.add root () OrderedPuzzles.empty in
  let visited = VisitedSet.empty in
  search frontier visited 50

let rec print_solution path = 
  match path with
  | puzzle :: path -> 
    print_puzzle puzzle;
    print_solution path
  | [] -> ()