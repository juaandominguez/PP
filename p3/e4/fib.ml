let rec fib n =
if n <= 1 then n
else fib (n-1) + fib (n-2);;

let rec fibArgv n = 
	if(n>0) then fibArgv(n-1)^"\n"^string_of_int(fib n)
else "0";;

if(Array.length(Sys.argv) <> 2) then print_endline("Número de argumentos inválido") else
print_endline(fibArgv(int_of_string(Sys.argv.(1))));
