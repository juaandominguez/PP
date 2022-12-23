
type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree
;;

let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r)
;;

let sum = 
	let aux x l r = x + l + r in fold_tree aux 0;;

let prod = 
	let aux x l r = x *. l *. r in fold_tree aux 1.0;;

let rec map_tree f = function
  | Empty -> Empty
  | Node (x,l,r) -> Node (f x, (map_tree f l), (map_tree f r));;

let size t = 
	let aux x l r = 1 + l + r in fold_tree aux 0 t;; 

let height t =
	let aux x l r = 1 + max l r in fold_tree aux 0 t;;

let inorder t =
	let aux x l r = l @ [x] @ r in fold_tree aux [] t;;

let mirror t =
	let aux x l r = Node (x, r, l) in fold_tree aux Empty t;;

