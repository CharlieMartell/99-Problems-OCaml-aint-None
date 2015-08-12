(* Reverse a list. *)

let rev l =
  let rec rrev rest = function
    | [] -> rest
    | x :: xs -> rrev (x::rest) xs
  in rrev [] l
;;

(* Tests all cases *)
assert (rev [`a; `b; `c; `d] = [`d; `c; `b; `a]);;
assert (rev [`a] = [`a]);;
assert (rev [] = []);;
