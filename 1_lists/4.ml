(* Find the number of elements of a list. *)

(* 
Examples from the book
# length [ "a" ; "b" ; "c"];;
- : int = 3
# length [];;
- : int = 0
*)

let length l = 
  let rec rlength i = function
      | [] -> 0
      | _ :: xs -> rlength (i + 1) xs
  in rlength 0 l
;;

(* Tests all cases *)
assert (length [ `a; `b; `c; `d] = 4);;
assert (length [`a] = 1);;
assert (length [] = 0);;
