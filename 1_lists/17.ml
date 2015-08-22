(* Split a list into two parts; the length of the first part is given. *)


(* 
Examples from the book
# split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list * string list =
  (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
# split ["a";"b";"c";"d"] 5;;
- : string list * string list = (["a"; "b"; "c"; "d"], [])
*)


(* Main Function *)
let rec split n lst =
  match lst , n with
  | _ , 0 -> ([] , lst) | [] , _ -> ([] , [])
  | x :: xs , _ -> (x :: (fst (split (pred n) xs)),
                   (snd (split (pred n) xs)))
;;

(* Tests all cases *)
assert(split 3 ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] =
  (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]));;
assert(split 2 ["a";"b";"c"] = (["a"; "b"], ["c"]));;
assert(split 8 ["a"; "b"; "c"] = ([], ["a"; "b"; "c"]));;
assert(split 0 [] = ([], []));;
