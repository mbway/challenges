let rec last_of = function
  | [] -> None
  | [x] -> Some x
  | _::xs -> last_of xs
;;
