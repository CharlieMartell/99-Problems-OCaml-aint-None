(* Flatten a nested list structure. *)

(* There is no nested list type in OCaml, so we need to define one
 * first. A node of a nested list is either an element, or a list of
 * nodes. *)

type 'a node =
  | One of 'a 
  | Many of 'a node list;;

(* 
Examples from the book
# flatten [ one "a" ; many [ one "b" ; many [ one "c" ; one "d" ] ; one "e" ] ];;
- : string list = ["a"; "b"; "c"; "d"; "e"]
*)

let rec flat lst =
  match lst with
    | [] -> []
    | (One x) :: xs -> x :: flat xs
    | (Many x) :: xs -> flat x @ flat xs
;;

(* Tests all cases *)
assert (flat [One `a; Many [One `b; Many [One `c; One `d]; One `e]] =
 [`a; `b; `c; `d; `e]);;
assert (flat [One `a] = [`a]);;
assert (flat [] = []);;
