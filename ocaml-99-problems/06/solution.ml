
let rec eq = function
  | ([],[])       -> true
  | (x::xs,y::ys) -> if x=y then eq (xs,ys) else false
  | (_,_)         -> false
;;

let is_palindrome = function
  | [] -> true;
  | xs -> let ys = List.rev xs in
  	eq (xs,ys)
;;

