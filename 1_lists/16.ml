(* Drop every N'th element from a list. *)

(* 
Examples from the book
# drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
*)

(* Main Function *)
let drop n lst = 
  let rec rdrop i lst = 
    match lst with 
    | [] -> []
    | x :: xs -> if i = n 
                 then rdrop 1 xs
                 else x :: rdrop (succ i) xs
  in rdrop 1 lst
;;

(* Tests all cases *)
assert(drop 3 ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] =
  ["a"; "b"; "d"; "e"; "g"; "h"; "j"]);;
assert(drop 2 ["a";"b";"c"] = ["a"; "c"]);;
assert(drop 1 ["a"] = []);;
assert(drop 5 [] = []);;
