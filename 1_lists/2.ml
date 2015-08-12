(* Find the last but one (last and penultimate) elements of a list. *)

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

