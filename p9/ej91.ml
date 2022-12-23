let to0from n =
	let rec aux i n = 
		if n < 0 then List.rev i else (aux(n::i)) (n-1)
	in aux [] n;;

let fromto m n =
	let rec aux i m n =
	if m > n then i else aux(n::i) m (n-1)
	in aux [] m n;;

let incseg l = 
  let rec aux l acc l2 = match l with
	| [] -> []
	| [h] -> List.rev ((h + acc)::l2)
	| h::t -> aux t (h + acc) ((h + acc)::l2)
  in aux l 0 [];;

let remove x l =
  let rec aux acc = function
	| [] -> l
	| h::t -> if x = h then List.rev_append acc t
	else aux (h::acc) t 
  in aux [] l ;;

let rec compress l = 
	let rec aux i l = match l with
	| h1::h2::t -> if h1 = h2 then aux i (h2::t) else aux (h1::i) (h2::t)
	| l -> List.rev_append i l
	in aux [] l;;
