let rec power' x y = (*y>=0*)
        if y=0 then 1 else
        if y mod 2 = 0 then power' (x*x) (y/2) else
        x * power' (x*x) (y/2);;

(*let powmod m b e = power' b e mod m;;*)

let rec powmod m b e = 
        if e=0 then 1 mod m
        else
        let 
        if (e>2) then (powmod m b e-2) * (powmod m b e-1)mod m
        else power' b e mod m;;



let rec powmod m b e = 
        if e=0 then 1 mod m
        else
        let p = powmod m ((b*b)mod m) (e/2) in
        if e mod 2=0 then p
        else b*p mod m

let powmod m b e = powmod m (b mod m) e;;
