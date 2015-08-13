(* Eliminate consecutive duplicates of list elements. *)

(* 
Examples from the book
# compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
*)

let rec comp lst = 
  match lst with
  | [] -> [] 
  | [x] -> [x]
  | x :: y :: xs -> if x = y
                    then comp (x::xs)
                    else x :: comp (y::xs)
;;

(* Tests all cases *)
assert (comp [`a; `a; `b; `c; `d] = [`a; `b; `c; `d]);;
assert (comp [`a] = [`a]);;
assert (comp [] = []);;
