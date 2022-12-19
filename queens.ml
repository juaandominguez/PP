(*Hecho por Juan Domínguez, juan.dominguezr@udc.es*)
let threatens (i1,j1) (i2,j2) = 
    i1 = i2 || 
    j1 = j2 ||
    abs (i2-i1) = abs (j2-j1);;
	
let rec compatible p l = 
    not (List.exists (threatens p) l);; 
	
let simplify sol = 
    List.map snd (List.sort compare sol);;

let foo n l = 
    let rec aux i = function 
    | [] -> List.rev i
    | h::t -> aux ((n+1-h)::i) t
    in aux [] l;;

let get_sol n =
    let bar = n mod 2 <> 0 in
    let rec sF p (i,j) =
        if i > n then [simplify p] 
         else if (j > n)||(i=1 && if bar then j>(n/2+1) else j>(n/2))||(bar && i=2 && j>((n/2)+1)&& snd(List.hd p)=(n/2+1)) then [] 
        else if compatible (i,j) p then
            List.rev_append(List.rev(sF p (i,j+1))) (sF ((i,j)::p) (i+1,1))
        else sF p (i,j+1)
    in sF [] (1,1);;  

let all_queens n = 
    if n > 1 then
    let rec aux i = function
    | [] -> i
    | h::t -> aux ([foo n h] @ [h] @ i) t
    in aux [] (get_sol n)
else get_sol n;; 

