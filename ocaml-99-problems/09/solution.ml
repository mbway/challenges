
let pack l =
	let rec aux inacc acc = function
	  | [] -> inacc::acc
	  | x::xs -> (match inacc with
	    | [] -> aux [x] acc xs
	    | y::_ -> if x=y
	    	then aux (x::inacc) acc xs
		else aux [x] (inacc::acc) xs)
	in
List.rev (aux [] [] l)
;;

(* my alternative solution *)
let pack' l =
	let rec aux cur curacc acc = function
	  | [] -> curacc::acc
	  | x::xs -> if x=cur
	  	then aux cur (x::curacc) acc xs
		else aux x [x] (curacc::acc) xs
	in
match l with
  | [] -> [] (* without this, result is [[]] *)
  | x::xs -> List.rev (aux x [x] [] xs)
;;


(* alternative solution *)
let pack list =
	let rec aux current acc = function
	  | [] -> [] (* Can only be reached if original list is empty *)
	  | [x] -> (x :: current) :: acc
	  | a :: (b :: _ as t) ->
		if a = b then aux (a :: current) acc t
		else aux [] ((a :: current) :: acc) t in
List.rev (aux [] [] list);;

