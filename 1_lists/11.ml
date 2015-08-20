(* Modified Run-length encoding of a list. *)

(* Default type for modified run length encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

(* 
Examples from the book
# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
   Many (4, "e")]
*)

(* Helper functions from prior exercises *)

(* Reverse a list. *)
let rev l =
  let rec rrev rest = function
    | [] -> rest
    | x :: xs -> rrev (x::rest) xs
  in rrev [] l
;;

(* Pack consecutive duplicates of list elements into sublists. *)
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

(* Find the number of elements of a list. *)
let rec length l = 
  match l with
      | [] -> 0
      | _ :: xs -> 1 + length xs
;;

(* Find the k'th element of a list. *)
let rec kth k = function
  | [] -> None
  | x :: xs -> if k = 0
               then Some x 
               else kth (k - 1) xs
;;

(* Main Function *)
(* Uses idea of right associative cons *)
let encode lst =
  let hd = function 
    [] -> failwith "hd"
    | a::l -> a
  in
    let rec rencode lst =
      match lst with
        | [] -> []
        | ([] :: xs) -> rencode xs
        | ((x :: []) :: xs) -> (One x) :: (rencode xs)
        | (x :: xs) -> (Many (length x, hd x)) :: (rencode xs)
      in rencode( pack (lst))
;;

(* Tests all cases *)
assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
   Many (4, "e")]);;
assert (encode ["a"; "a"; "b"; "c"] = [Many (2, "a"); One "b"; One "c"]);;
assert (encode ["a"] = [One "a"]);;
assert (encode ["a";"a"] = [Many(2, "a")]);;
assert (encode [] = []);;
