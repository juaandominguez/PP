let rec fact n =
  if n > 0 then n * fact (n-1)
  else 1;;
  let comb (m, n) =
  fact m / fact (m-n) / fact n;;