let rec at k xs = match (k,xs) with
 | (_, []) -> None
 | (0, x::_) -> Some x
 | (k, x::xs) -> at (k-1) xs
;;

(* alternative solution *)
let rec at k = function
 | [] -> None
 | x::xs -> if k=0 then Some x else at (k-1) xs
;;
