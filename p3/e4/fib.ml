let rec fib n =
if n <= 1 then n
else fib (n-1) + fib (n-2);;
let rec fibArgv n = 
	if(n<=int_of_string(Sys.argv.(1))) then (print_endline(string_of_int(fib n)); 
			fibArgv (n+1));;


if(Array.length(Sys.argv) <> 2) then print_endline("Número de argumentos inválido") else
fibArgv(0);
(*let rec print_fibs n =
if n<0 then ()
	else (print_fibs(n-1); print_fib(n));;*)