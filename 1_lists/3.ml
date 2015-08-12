(* Find the k'th element of a list. *)

(* 
Examples from the book
# at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
- : string option = Some "c"
# at 3 [ "a" ];;
- : string option = None
*)

let rec kth k = function
  | [] -> None
  | x :: xs -> if k = 0
               then Some x 
               else kth (k - 1) xs
;;

(* Tests all cases *)
assert (kth 2 [ `a; `b; `c; `d] = Some `c);;
assert (kth 0 [`a] = Some `a);;
assert (kth 2 [ `a; `b]  = None);;
assert (kth 2 [] = None);;
