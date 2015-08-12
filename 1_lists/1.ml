(* Write a function last : 'a list -> 'a option 
 * that returns the last element of a list. *) 

(* 
Examples from the book
# last [ "a" ; "b" ; "c" ; "d" ];;
- : string option = Some "d"
# last [];;
- : 'a option = None
*)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: y -> last y
;;

(* Tests all cases *)
assert (last [ `a; `b; `c]  = Some `c);;
assert (last [`a] = Some `a);;
assert (last [] = None);;
