(* Decuode a Modified Run-length encoding list. *)

(* Default type for modified run length encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

(* 
Examples from the book
# decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
- : string list =
  ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
*)

(* Helper functions from prior exercises *)

(* Reverse a list. *)
let rev l =
  let rec rrev rest = function
    | [] -> rest
    | x :: xs -> rrev (x::rest) xs
  in rrev [] l
;;

(* Main Function *)
let decode lst =
  let rec rdecode acc = function
    | [] -> acc
    | One (x) :: xs -> rdecode (x :: acc) xs
    | Many (n, x) :: xs -> if n > 0 
                          then rdecode (x :: acc) (Many (n - 1, x) :: xs)
                          else rdecode acc xs
  in rev (rdecode [] lst) 
;;

(* Tests all cases *)
assert (decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
   Many (4, "e")] = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]);;
assert (decode [Many (2, "a"); One "b"; One "c"] = ["a"; "a"; "b"; "c"]);;
assert (decode [One "a"] = ["a"] );;
assert (decode [Many(2, "a")] = ["a";"a"]);;
assert (decode [] = []);;
