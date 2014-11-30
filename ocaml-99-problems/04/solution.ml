
(* not tail recursive *)
let rec len = function
  | []    -> 0
  | _::xs -> 1 + len xs
;;

(* tail recursive *)
let len' xs =
	let rec len'' acc = function
	  | []    -> acc
	  | _::xs -> len'' (acc+1) xs
	in
len'' 0 xs
;;
