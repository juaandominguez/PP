let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;

(*let rec orbit n =
	if n = 1 then string_of_int(n)
	else String.concat ", " [string_of_int(n); orbit(f n)];; *)

	let rec orbit n =
		if n = 1 then string_of_int(n)
		else string_of_int(n)^", "^orbit(f n);;	

let rec length n = 
	if n = 1 then 0
	else 1 + length(f n);; 

let rec top n = 
	if n = 1 then 1
		else max n (top(f n));;

let rec length'n'top n =
	if n = 1 then (0,1)
	else let i,j = length'n'top(f n)
	in (i+1, max n j);;

let rec longest_in m n=
	let rec check_from i=
			if i <= m then m, length m
			else let j,length_j= check_from (i-1) in
			let length_i = length i in
			if length_j >= length_i then j,length_j else i,length_i
	in check_from n;;

	let rec highest_in m n=
	let rec check_from i=
			if i <= m then m, top m
			else let j,top_j= check_from (i-1) in
			let top_i = top i in
			if top_j >= top_i then j,top_j else i,top_i
	in check_from n;;