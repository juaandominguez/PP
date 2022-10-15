let rec power x y = (*y>=0*)
	if y=0 then 1 else
	x * power x (y-1);;

let rec power' x y = (*y>=0*)
	if y=0 then 1 else
	if y mod 2 = 0 then power' (x*x) (y/2) else
	x * power' (x*x) (y/2);;
(*power' es mas eficiente que power ya que reduce el número de recursiones necesarias,
pero dentro del rango de los int la diferencia en esta funcion no es percetible*)

let rec powerf x y = (*y>=0*)
	if y=0 then 1. else
	if y mod 2 = 0 then powerf (x *. x) (y/2) else
	x *. powerf (x *. x) (y/2);;