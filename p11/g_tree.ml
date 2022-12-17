
type 'a g_tree =
  Gt of 'a * 'a g_tree list
;;

let rec size = function 
| Gt (_, []) -> 1
| Gt (r, h::t) -> size h + size (Gt (r, t));;

let rec height = function
| Gt (_,[]) -> 0
| Gt (r,h::t)-> 1 + height (Gt(r,t));;

let rec leaves = function 
    Gt(r,[]) -> [r]
  | Gt(r,l) -> List.flatten (List.map leaves l);;

  let rec mirror = function 
  Gt (r,l) -> Gt(r, List.map mirror (List.rev l));;

let rec preorder



let rec postorder