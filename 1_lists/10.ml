(* Run-length encoding of a list. *)

(* 
Examples from the book
# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : (int * string) list =
  [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
*)

(* Helper function from prior exercise *)
let rev l =
  let rec rrev rest = function
    | [] -> rest
    | x :: xs -> rrev (x::rest) xs
  in rrev [] l
;;

(* Main function *)
let encode lst =
    let rec rencode sum lst =
      match sum, lst with
        | _, [] -> sum
        | [], x::xs -> rencode [(1, x)] xs
        | (num, y)::ys, x::xs -> if x = y
                                 then rencode (((num + 1), y)::ys) xs
                                 else rencode ((1, x)::sum) xs
      in rev (rencode [] lst)
;;

(* Tests all cases *)
assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
  [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);;
assert (encode ["a"; "a"; "b"; "c"] = [(2, "a"); (1, "b"); (1,"c")]);;
assert (encode ["a"] = [(1,"a")]);;
assert (encode [] = []);;
