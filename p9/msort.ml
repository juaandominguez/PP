let rec divide l = match l with
h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
| _ -> l, [];;

let rec merge = function
[], l | l, [] -> l
| h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, h2::t2)
else h2 :: merge (h1::t1, t2);;

let rec msort1 l = match l with
[] | _::[] -> l
| _ -> let l1, l2 = divide l in
merge (msort1 l1, msort1 l2);;

let l2 = List.init 150_000 (function _ -> Random.int 10000000);;

(*Con l2 se produce un stack overflow*)

let divide' l =
  let rec aux div1 div2 = function
      [] -> (List.rev div1, List.rev div2)
    | h::[] -> (List.rev (h::div1), List.rev div2)
    | h1::h2::t -> aux (h1::div1) (h2::div2) t
  in aux [] [] l;;

let merge' ord (l1, l2) =
  let rec aux (i, j) acc = match i, j with
    | [], l | l, [] -> List.rev_append acc l
    | h1::t1, h2::t2 -> if ord h1 h2 then aux (t1, h2::t2) (h1::acc)
    else aux (h1::t1, t2) (h2::acc)
  in aux (l1, l2) [];;

let rec msort2 ord l = match l with
  | [] | _::[] -> l
  | _ -> let l1, l2 = divide' l in
  merge' ord (msort2 ord l1, msort2 ord l2);;

let crono f x=
  let t=Sys.time() in
  let _= f x in
  Sys.time()-. t;;

let rec rep n f x=
  if n=0 then ()
  else (f x;rep (n-1) f x);;
  
let listas=
List.map (function x->List.init x(function _-> Random.int 10_000_000))[40_000;50_000;75_000;100_000];;

let t1=List.map(function l->crono (rep 5 (msort1)) l) listas;;

let t2=List.map(function l-> crono (rep 5 (msort2 (<=)))l) listas;;

List.map2 (fun x y -> (x/.y)) t1 t2;;

(*Observamos que msort1 es aproximadamente un 20% más rápido que msort2*)
