
let rec fact n = 
    if n > 0 then n * fact (n-1)
    else 1;;

let rec truncatedFact n m=
    if n>m then n* truncatedFact(n-1) m
    else 1;;
    
let comb (m, n) = 
    (truncatedFact m (m-n))/fact n;;

