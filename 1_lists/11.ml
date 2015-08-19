(* Modified Run-length encoding of a list. *)

(* Default type for modified run length encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

(* 
Examples from the book
# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
   Many (4, "e")]
*)

let encode lst =
  let rev l =
    let rec rrev rest = function
      | [] -> rest
      | x :: xs -> rrev (x::rest) xs
    in rrev [] l
  in 
    let rec rencode sum lst =
      match sum, lst with
        | _, [] -> sum
        | [], x::xs -> rencode [(1, x)] xs
        | (num, y)::ys, x::xs -> if x = y
                                 then rencode (((num + 1), y)::ys) xs
                                 else rencode ((1, x)::sum) xs
      in rev (rencode [] lst)
;;
