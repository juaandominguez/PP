let factors n =
  let rec aux i n=
  if n = 1 then [] else
    if n mod i = 0 then i::aux i (n/i)
    else aux (i+1) n
    in aux 2 n;;   
    
let factors2 n =
  let rec aux i n j=
    if n = 1 && j = 0 then [] else
      if n<>0 && n mod i = 0 then aux i (n/i) (j+1)
      else if j>0 then (i,j)::aux (i+1) n 0 else aux (i+1) n 0
    in aux 2 n 0;; 

let is_prime n =
  let n = max n (-n) in
   let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
   in is_not_divisor 2;;

let rec all_primes x y =
  let rec aux i =
    if i>y then []
    else if is_prime i then i::aux(i+1)
    else aux (i+1)
  in aux x;;

let goldbach n =
  let rec aux i j =
    if not (is_prime i) then aux(i+1) j else
      if not (is_prime j) then aux i (j+1) else
        if i+j = n then (i,j) else
          if j>=(n-2) then aux(i+1) 1 else aux i (j+1)
        in aux 2 2;;

let rec goldbach_list x y =
    let rec aux x y =
      if x>y then [] else
        if x mod 2 = 0 then
        (x,(goldbach x))::aux (x+1) y
        else aux (x+1) y
      in aux x y;;

let rec last_two l = match l with
| [] | [_]-> raise(Failure("Not enough elements"))
| [x;y] -> x,y
| h::t -> last_two t;;

let rev l =
  let rec aux i l= match l with
    | [] -> i
    | h :: t -> aux (h :: i) t
  in
  aux [] l;;

 let rev l= 
 let rec aux i l = match l with
 |[]->i 
 |h::t -> aux(h::i) t
in aux [] l;;
