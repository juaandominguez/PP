let hd l = match l with
  [] -> raise(Failure("hd"));
| h::t -> h;;

let tl l = match l with
  [] -> raise(Failure("tl"));
| h::t -> t;;

let length l = 
    let rec aux i l = match l with
| [] -> i
| h::t -> aux(i+1) t
in aux 0 l;;

let compare_lengths l1 l2 = match l1,l2 with
| [],[] -> 0
| [],_ -> -1
| _,[] -> 1
| _,_ -> if length l1 > length l2 then 1 else if length l1 < length l2 then -1
else 0;;

let nth l n = 
if n<0 then raise(Failure("Invalid_argument"))
else if n>length l then raise(Failure("List is too short")) else
let rec aux l n = 
  if n=0 then (hd l)
  else aux (tl l) (n-1)
in aux l n;;

let rec append l1 l2 = match l1 with
| [] -> l2
| h::t -> h::append l2 t;; 

let rec find f l = match l with
  [] -> raise(Failure("Not_found"))
| h::t when not(f h) -> find f t
| h::t when (f h) -> h;;

let rec for_all f l = match l with
  [] -> true
| h::t -> f h; for_all f t;;