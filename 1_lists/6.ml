(* Checks if a list is a palindrome *)

(* 
Examples from the book
# is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
- : bool = true
# not (is_palindrome [ "a" ; "b" ]);;
- : bool = true
*)

let pal l =
  let rev l =
    let rec rrev rest = function
      | [] -> rest
      | x :: xs -> rrev (x::rest) xs
    in rrev [] l
  in rev l = l
;;

(* Tests all cases *)
assert (pal [`a; `b; `c; `d] = false);;
assert (pal [`a; `b; `b; `a] = true);;
assert (pal [`a; `b; `a] = true);;
assert (pal [`a] = true);;
assert (pal [] = true)

