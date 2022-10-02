(*1-*)let rec sum_cifras n = (*n>=0*)
		if n>0 then (n mod 10) + sum_cifras(n/10)
		else 0;; 

(*2-*)
let rec num_cifras n = (*n>=0*)
	if n>0 then 1 + num_cifras(n/10)
	else 0;; 

(*3-*)
let rec exp10 n = (*n>=0*)
	if n=0 then 1 else 10 * exp10 (n-1);;

(*4-*)
let rec reverse n = (*n>=0*)
	if n >= 10 then (exp10((num_cifras n) - 1))*(n mod 10) + reverse (n/10) else (n mod 10);; 
