
open Bin_tree;;

let rec insert_tree ord x = function
	| Empty -> Node(x,Empty,Empty)
	| Node (r,left,right) -> if ord x r then
	Node (r, insert_tree ord x left, right)
	else Node (r, left, insert_tree ord x right) ;;

let tsort ord l =
  inorder (List.fold_left (fun a x -> insert_tree ord x a) Empty l)
;;

