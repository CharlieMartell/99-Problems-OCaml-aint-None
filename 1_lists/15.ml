(* Replicate the elements of a list a given number of times. *)


(* 
Examples from the book
# replicate ["a";"b";"c"] 3;;
- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
*)

(* Main Function *)
let rec replicate n lst = 
  let rec wdup n x = if n > 0 then x :: (wdup (pred n) x) else []
  in 
    match lst with 
    | [] -> []
    | x :: xs -> wdup n x @ replicate n xs
;;

(* Tests all cases *)
assert(replicate 3 ["a";"b";"c"] = 
  ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]);;
assert(replicate 2 ["a"] = ["a"; "a"]);;
assert(replicate 5 [] = []);;
