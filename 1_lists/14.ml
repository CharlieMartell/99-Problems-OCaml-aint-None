(* Duplicate the elements of a list. *)

(* 
Examples from the book
# duplicate ["a";"b";"c";"c";"d"];;
- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
*)

(* Main Function *)
let rec duplicate lst = 
  match lst with
  | [] -> []
  | x :: xs -> x :: [x] @ duplicate xs
;;

(* Tests all cases *)
assert(duplicate ["a";"b";"c";"c";"d"] = 
  ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]);;
assert(duplicate ["a"] = ["a"; "a"]);;
assert(duplicate [] = []);;
