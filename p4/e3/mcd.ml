let rec mcd x y = 
	if y>x then mcd y x else
	if y > 0 then mcd (x mod y) y else x;;