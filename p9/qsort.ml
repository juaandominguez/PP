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

let l1 = List.init 400000 (function _ -> Random.int 10000000);;

(*Con qsort1 se produce un stack overflow mientras que qsort2 consigue ordenar la lista*)

let crono f x=
  let t=Sys.time() in
  let _= f x in
  Sys.time()-. t;;

let rec rep n f x=
  if n=0 then ()
  else (f x;rep (n-1) f x);;
  
let listas=
List.map (function x->List.init x(function _-> Random.int 10_000_000))[75_000;100_000;150_000;200_000];;

let t1=List.map(function l->crono (rep 5 (qsort1 (<=))) l) listas;;

let t2=List.map(function l-> crono (rep 5 (qsort2 (<=)))l) listas;;

List.map2 (fun x y -> (x/.y)) t1 t2;;

(*Observando la diferencia de tiempos, hallamos que qsort1 es aproximadamente 
un 10% más rápido que qsort2, lo cual es una ventaja para el primero.
Esto se debe a que para qsort2 se usa una definicion de append recursiva terminal,
la cual voltea la lista 2 veces, por lo tanto es menos eficiente.*)

