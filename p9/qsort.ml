let rec qsort1 ord = function
[] -> []
| h::t -> let after, before = List.partition (ord h) t in
qsort1 ord before @ h :: qsort1 ord after;;

let rec qsort2 ord =
  let append' l1 l2 = List.rev_append (List.rev l1) l2 in
  function
  [] -> []
  | h::t -> let after, before = List.partition (ord h) t in
  append' (qsort2 ord before) (h :: qsort2 ord after);;


(*La ventaja de qsort2 respecto a qsort1 es que tiene una recursividad
terminal lo cual permite realizar operaciones con listas con un mayor
número de elementos sin que se produzca un stack overflow*)
(*Podemos observar que para *)

let l1 = List.init 400000 (function _ -> Random.int 10000000);;

let crono f x =
  let t = Sys.time() in 
  let _ = f x in
  Sys.time () -. t;;


let l = List.init 150_000 (function _ -> Random.int 10_000_000);;

let t1 = crono (qsort1 (<=)) l;;
let t2 = crono (qsort2 (<=)) l;;



