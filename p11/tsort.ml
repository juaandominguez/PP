
open Bin_tree;;

let rec insert_tree ord x = ... ;;

let tsort ord l =
  inorder (List.fold_left (fun a x -> insert_tree ord x a) Empty l)
;;

