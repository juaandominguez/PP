let rec power' x y = (*y>=0*)
        if y=0 then 1 else
        if y mod 2 = 0 then power' (x*x) (y/2) else
        x * power' (x*x) (y/2);;

(*let powmod m b e = power' b e mod m;;*)

let rec powmod m b e = 
        if (e>2) then (powmod m b e-2) * (powmod m b e-1)mod m
        else power' b e mod m;;


