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

let rec longest_in m n =
	if m = n then m, length m
	else
	if m > n then longest_in n m
	else let rec check_from i = 
		if max (length m) (length i) > length m then longest_in i n
		else check_from (i+1)
	in check_from m;;

