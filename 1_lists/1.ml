(*Write a function last : 'a list -> 'a option 
 * that returns the last element of a list.*) 

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: y -> last y
;;

assert (last [ `a; `b; `c]  = Some `c);;
assert (last [`a] = Some `a);;
assert (last [] = None);;
