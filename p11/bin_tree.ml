
type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree
;;

let rec map_tree f = ... ;;

let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r)
;;

let sum t = ... ;;

let prod t = ... ;;

let size t = ... ;;

let height t = ... ;;

let inorder t = ... ;;

let mirror t = ... ;;

