let longest_in m n =
	let rec aux = (imax,lmax), i
	if i>n then (imax,lmax)
	else let l = length i in
	if(l>lmax) then aux(i,lmax) i+1
	else aux(imax,lmax) i+1 
	in aux = (m,length m) (m+1);;