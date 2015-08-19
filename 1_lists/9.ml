(* Pack consecutive duplicates of list elements into sublists. *)

(* 
Examples from the book
# pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
- : string list list =
  [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
   ["e"; "e"; "e"; "e"]]
*)

(* Helper function from prior exercise *)

(* Reverse a list. *)
let rev l =
  let rec rrev rest = function
    | [] -> rest
    | x :: xs -> rrev (x::rest) xs
  in rrev [] l
;;

(* Main function *)
let pack lst =
  let rec rpack pre suf cur_val lst =
     match lst with
       | [] -> rev (suf :: pre)
       | x :: xs when x = cur_val -> rpack pre (x::suf) cur_val xs
       | x :: xs -> rpack (suf::pre) [x] x xs
  in match lst with
    | [] -> []
    | x :: xs -> rpack [] [x] x xs
;;

(* Tests all cases *)
assert (pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] =
             [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
             ["e"; "e"; "e"; "e"]]);;
assert (pack ["a"] = [["a"]]);;
