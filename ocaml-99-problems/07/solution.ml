(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
type 'a node =
  | One of 'a 
  | Many of 'a node list
;;

(* when pattern matching, One x :: t matches x to type One, but does not match t to anything *)
let flatten l =
	let rec flatten' acc = function
	  | []             -> acc
	  (* matches this kind of thing: [One "a"; THE_REST] *)
	  | One x :: rest  -> flatten' (x::acc) rest
	  (* matches this kind of thing: [Many [One "a"; One "b"]; THE_REST] *)
	  | Many x :: rest -> flatten' (flatten' acc x) rest
	in
List.rev (flatten' [] l)
;;

(* I think I've got it. The outer most list has to have homogeneous type elements.
   The type is either a list of Ones or Manys
   eg. [1;2;3] --> [One 1; One 2; One 3]
   eg. (if this was valid (it's not and that's why the type is needed in the first place)
   	[1;[2;3];4] --> [One 1; Many [One 2; One 3]; One 4]
*)
flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;

