let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;

let rec orbit n =
	if n = 1 then string_of_int(n)
	else String.concat ", " [string_of_int(n); orbit(f n)];;

let rec length n = 
	if n = 1 then 0
	else 1 + length(f n);; 

let rec top n = 
	if n = 1 then 1
		else max n (top(f n));;



