(* compress [1;2;3;3;3;3;4;4;4;5;6;6;7] = [1; 2; 3; 4; 5; 6; 7] *)
let rec compress = function 
  | [] -> []
  | x::rest -> let rest = compress rest in match rest with
    | [] -> [x]
    | y::ys -> if x=y then rest else x::rest
;;


(* alternative solution *)
let rec compress' = function
  | [] -> []
  | [x] -> [x]
  	(* this sets t = y::_, not t=_ like I initially thought *)
  | x :: (y :: _ as t) -> let r = compress' t in
  	if x=y then r else x::r
;;
