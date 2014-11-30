
(* not tail recursive *)
let rec rev = function
  | []    -> []
  | x::xs -> (rev xs) @ [x]
;;

(* tail recursive *)
let rev' xs =
	let rec rev'' acc xs = match xs with
	  | []    -> acc
	  | x::xs -> rev'' (x::acc) xs
	in
rev'' [] xs
;;
