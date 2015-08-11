(*Write a function last : 'a list -> 'a option 
 * that returns the last element of a list.*) 

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: y -> last y;;

