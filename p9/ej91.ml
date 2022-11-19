let to0from n =
	let rec aux i n = 
		if n < 0 then List.rev i else (aux(n::i)) (n-1)
	in aux [] n;;

let fromto m n =
	let rec aux i m n =
		if m > n then i else aux(n::i) m (n-1)
	in aux [] m n;;

	(*TODO*)
let incseg l =
	let rec aux i l = match l with
	  [] -> i
	| h1::h2::t -> aux(h1+h2::i) t
	| h1::t -> aux(h1::i) t
in aux [] l;;

let remove x l = 
let rec aux i x l = match l with 
[] -> i
| h::t -> if x = h then List.rev_append i t  else aux (h :: i) x t
in aux [] x l;;

let rec compress l = 
let rec aux i l = match l with
| h1::h2::t -> if h1 = h2 then aux i (h2::t) else aux (h1::i) (h2::t)
| l -> List.rev_append i l
in aux [] l;;
