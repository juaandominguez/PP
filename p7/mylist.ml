let hd l = match l with
  [] -> raise(Failure("hd"));
| h::t -> h;;

let tl l = match l with
  [] -> raise(Failure("tl"));
| h::t -> t;;

let length l = 
    let rec aux i l = match l with
| [] -> i
| h::t -> aux(i+1) t
in aux 0 l;;

let compare_lengths l1 l2 = match l1,l2 with
| [],[] -> 0
| [],_ -> -1
| _,[] -> 1
| _,_ -> if length l1 > length l2 then 1 else if length l1 < length l2 then -1
else 0;;

let nth l n = 
if n<0 then raise(Failure("Invalid_argument"))
else if n>length l then raise(Failure("List is too short")) else
let rec aux l n = 
  if n=0 then (hd l)
  else aux (tl l) (n-1)
in aux l n;;

let rec append l1 l2 = match l1 with
| [] -> l2
| h::t -> h::append l2 t;; 

let rec find f l = match l with
  [] -> raise(Failure("Not_found"))
| h::t -> if f h then h else find f t;;

let rec for_all f l = match l with
  [] -> true
| h::t -> (f h)&&(for_all f t);;

let rec exists f l = match l with
  [] -> false
| h::t -> (f h)||(exists f t);;

let rec mem a l = match l with
  [] -> false
| h::t -> if h = a then true else mem a t;; 

let rec filter f l = match l with
  [] -> []
| h::t -> if f h then h::(filter f t) else (filter f t);;

let rec find_all f l = match l with
  [] -> []
| h::t -> if f h then h::(find_all f t) else (find_all f t);; 

let partition f l = (*Cambiar*)
  let negated f x = not (f x) in
  find_all f l, find_all (negated f) l;;

  let split l = (*not valid**)
  let rec aux i j l = match l with
  [] -> i,j
  | h::t -> aux ((fst h)::i) ((snd h)::j) t
  in aux [] [] (List.rev l);; 

  let split l = match l with
  [] -> [],[]
  |[x,y]::_ -> x,y

  let split_rev l = 
    let rec aux i j l = match l with
    [] -> i,j
  | h::t -> aux ((fst h)::i) ((snd h)::j) t
    in aux [] [] l;; 

    let rev l= 
    let rec aux i l = match l with
    |[]->i 
    |h::t -> aux(h::i) t
   in aux [] l;;

      let rec combine l1 l2 =
        match l1,l2 with
        | [],[] -> []
        | [],_ | _,[] -> raise(Invalid_argument"Different lengths")
        | h1::t1,h2::t2 -> (h1,h2)::(combine t1 t2);;

let init len f =
  if len < 0 then raise(Invalid_argument"Invalid length")
  else let rec aux i =
      if i<len then f i::(aux (i+1))
      else []
    in aux 0;;

let rev_append l1 l2 = 
  let rec aux i l1 = match l1 with
    | [] -> i@l2
    | h::t -> aux(h::i) t
  in aux [] l1;;

let rec concat l = match l with
      [] -> []
    | h::t -> h@concat t;;

let rec flatten l = match l with
  [] -> []
  | h::t -> h@flatten t;;

let rec map f l = match l with
| [] -> []
| h::t -> (f h)::map f t;;

let rev_map f l = 
    let rec aux i l = match l with
      [] -> i
    | h::t -> aux((f h)::i) t
    in aux [] l;; 

let rec map2 f l1 l2 = match l1,l2 with
| [],[] -> []
| [],_ | _,[] -> raise(Invalid_argument"map2")
| h1::t1,h2::t2 -> f h1 h2 :: map2 f t1 t2;;

let fold_left f a l = 




let fold_right









  