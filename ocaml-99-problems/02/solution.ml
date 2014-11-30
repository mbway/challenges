let rec last_two = function
  | [] -> None
  | [x] -> None
  | x::y::[] -> Some (x,y)
  | x::xs -> last_two xs
;;

(* better solution *)
let rec last_two = function
 | [] | [_] -> None
 | [x;y] -> Some (x,y)
 | x::xs -> last_two xs
;;
