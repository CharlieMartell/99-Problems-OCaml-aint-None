(* Find the k'th element of a list. *)

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
