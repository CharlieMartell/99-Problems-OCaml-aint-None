(* Find the number of elements of a list. *)

(* 
Examples from the book
# length [ "a" ; "b" ; "c"];;
- : int = 3
# length [];;
- : int = 0
*)

let rec length l = 
  match l with
      | [] -> 0
      | _ :: xs -> 1 + length xs
;;

(* Tests all cases *)
assert (length [ `a; `b; `c; `d] = 4);;
assert (length [`a] = 1);;
assert (length [] = 0);;
