open Printf;;
open Batteries;;

type cell = Dead | Alive;;
type state = State of cell list list;;
type coord = Coord of (int * int);;

let height state = List.length state;;

let width state = List.length (List.hd state);;

(* convert one row of state to a string *)
let row_string row =
  row
  |> List.map (fun c -> match c with
                        | Alive -> "#"
                        | Dead -> "-")
  |> List.append ["\n"]
  |> String.concat "";;

(* print the state neatly *)
let print_state state =
  state
  |> List.map row_string
  |> String.concat ""
  |> print_string;;

(* return (int * int) list list
 * of all combintions of the given elements *)
let combinations_grid xs ys =
  List.map (fun y -> List.map (fun x -> x,y) xs) ys;;

(* direcitions to the 8 neigbors *)
let dirs = [-1; 0; 1];;
let offsets =
  combinations_grid dirs dirs
  |> List.concat
  |> List.filter (fun t -> match t with
                           | (0,0) -> false
                           | _ -> true);;
                        

(* neighbor coords for a cell *)
let neighbor_coords state (x, y) =
  offsets (* add offsets to given coord *)
  |> List.map (fun (ox, oy) -> x+ox, y+oy)
  (* remove coords outside grid *)
  |> List.filter (fun (_, cy) ->
                      0 <= cy &&
                      cy < height state)
  |> List.filter (fun (cx, _) ->
                      0 <= cx &&
                      cx < width state);;

(* true if element at coord x,y is alive *)
let is_alive state (x, y) =
  (List.nth (List.nth state y) x) = Alive;;

(* count alive cells from coords *)
let alive_coords_count state coords =
  List.fold_left (fun count coord ->
                      match is_alive state coord with
                      | true -> count + 1
                      | false -> count) 0 coords;;

(* count the number of alive neighbors *)
let alive_neighbors_count state cellcoord =
  alive_coords_count state (neighbor_coords state cellcoord);;

(* next step of a single cell *)
let next_cell_state state coord =
  match is_alive state coord with
  | true -> (match alive_neighbors_count state coord with
             | 0 | 1 -> Dead
             | 2 | 3 -> Alive
             | _ -> Dead)
  | false -> (match alive_neighbors_count state coord with
              | 3 -> Alive
              | _ -> Dead);;

(* get coord list list with size matching state *)
let coord_board state =
  combinations_grid
    (List.of_enum (0--(width state - 1)))
    (List.of_enum (0--(height state - 1)));;

(* next step of the entire board *)
let next_game_state state =
  coord_board state
  |> List.map (List.map (fun c -> next_cell_state state c));;

(* MAIN *)
let a = Alive;;
let d = Dead;;
let State state = State([
  [d; a; d; d; d; d; d];
  [d; d; a; d; d; d; d];
  [a; a; a; d; d; d; d];
  [d; d; d; d; d; d; d];
  [d; d; d; d; d; d; d];
]);;

(* DEBUG PRINTING *)
print_state state;;

let state = next_game_state state;;
print_state state;;

let state = next_game_state state;;
print_state state;;

let state = next_game_state state;;
print_state state;;
(*
print_string "step 2\n";;
let state = nextGameState state;;
printState state;;
*)
