(* Find the last but one (last and penultimate) elements of a list. *)

(* 
Examples from the book
# last_two [ "a" ; "b" ; "c" ; "d" ];;
- : (string * string) option = Some ("c", "d")
# last_two [ "a" ];;
- : (string * string) option = None
*)

let rec last_two = function
  | [] -> None
  | [_] -> None
  | [x;y] -> Some (x,y)
  | _ :: z -> last_two z
;;

(* Tests all cases *)
assert (last_two [ `a; `b; `c; `d]  = Some (`c, `d));;
assert (last_two [ `a; `b; `c]  = Some (`b, `c));;
assert (last_two [`a] = None);;
assert (last_two [] = None);;

