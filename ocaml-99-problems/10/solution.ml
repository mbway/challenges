let pack l =
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
(* List.hd already throws an error on the empty list so there
   is no need to handle the case where e is empty
*)
let encode l = List.map (fun e -> (List.length e, List.hd e)) (pack l);;

let encode' l =
	let aux curCount cur acc = function
	  | [] -> [] (* only reached if input is empty *)
	  | x::xs -> if x=cur
	  	then aux (curCount+1) cur acc xs
		else (match xs with
		  | [] -> (1,x)::(curCount, cur)::acc
		  | x'::xs' -> aux 1 x' ((curCount, cur)::acc) xs')
	in
match l with
  | [] -> 
  | x::xs -> List.rev (aux 1 x [] xs)
;;
