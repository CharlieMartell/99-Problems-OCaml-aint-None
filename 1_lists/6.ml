(* Checks if a list is a palindrome *)

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

