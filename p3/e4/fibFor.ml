let rec fib n =
if n <= 1 then n
else fib (n-1) + fib (n-2);;

if(Array.length(Sys.argv) <> 2) then print_endline("Número de argumentos inválido") else
for i=0 to int_of_string(Sys.argv.(1)) do print_endline(string_of_int(fib(i))) done;;