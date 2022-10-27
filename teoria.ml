(*16/09/2022*)
let alltrue = function x -> true;;
let alltruetyped = function (x:bool)-> true;;
let alltrue x = true;;
let alltrue _ = true;; (*Better*)
let x = 1 in 2 * 3;;
let id = function x -> x;;
let not_zero = function 0 -> false | _ -> true;;
(*21/09/2022*)
|> pipe
match <e> with p1->e1 ...
let f = function x->
				 (function y ->x+y);;
let x y = x+y;;
(*28/09/2022*)
let max = function x,y -> if(x>=y) then x else y;;
let min x = function y -> if x < y then x else y;;
let min x y =  if x < y then x else y;;
let p = (true, ()), "falso trio";;
let p2 = true, (), "falso trio";;
let x, y = p;;
let second (x,y) = y;;
let second (_,y) = y;;
(*30/09/2022*)
open Num;;
let g x y = x+y,x*y;;
let rec quo x y = (* x >= 0 , y > 0*)
	if x<y then 0
	else 1 + quo (x-y) y;;

let rec rem x y =
	if x<y then x
	else rem (x-y) y;;

let div x y = quo x y, rem x y;;

let rec div x y = 
	if x<y then (0,x)
	else let q,r = div(x-y) y in 1 + q, r;;

(*05/10/2022*)
(*using fib (p3/e4)*)
let crono f x =
	let t = Sys.time() in
	let _ = f x in 
	Sys.time() -. t;;

2+1; "hola" ^ "adios";;

let crono f x =
	let t = Sys.time() in
	f x; 
	Sys.time() -. t;; (*mas facil*)
	let k = (1. +. sqrt 5.) /. 2.;;
	let t32 = crono fib 32;;

let rec fib2 = function
	1 -> 1,0
	| n -> let f1,f2 = fib2 (n-1) in
		f1 + f2, f1;;

[1;2;3];;
[1;100];;
['a';'e';'i']
let l = [1;2;3] (*int list*)
List.length l ;;
List.hd
List.tl
List.hd (List.tl (List.tl l))
let rec length l =
	if l = [] then 0 else
	1 + length (List.tl l);;
	<a'>::<a' list>

let rec from_to i j =
	if i<= j then i :: from_to (i+1) j
	else [];;

let from_to n m = 
	if n <= m then
	List.init (n-m+1) ((+)m)
	else [];;

[1;2;3] @ [100;1000];;
let l = 1::2::3::4::[];;
raise;;
let hd = function
| h::_ -> h
| [] -> raise(Failure "hd");;
let rec nth l x= 
if x<0 then raise(Failure("hd"))
else if x=0 then List.hd(l)
else nth (List.tl(l)) (x-1);;

let rec nth l x= match (l,x) with
([],_) -> raise(Failure("nth"))
| (h::_,0) -> h
| (_::t,n) -> nth t (x-1)

let nth l x =
	if x>= 0 then nth l x 
else raise(Failure("nth"));

let append a b = l2::l1::[];; (*lista de a'*)

let rec append l1 l2 =
	if l1 = [] then l2
	else List.hd l1 ::append(List.tl l1)l2;; 

let rec append l1 l2 = match l1 with
[] -> l2
| h::t -> h::append t l2;;

let rec append = function
[] -> (function l -> l)
| h::t -> (function l -> h::append t l);;

List.compare_lengths;;

let compare_lengths l1 l2 =
	compare (List.length l1)(List.length l2);;

let rec compare_lengths l1 l2 = match l1,l2 with
  [],[] -> 0
| [],_ -> -1
| _,[] -> 1
| _::t1, _::t2 -> compare_lengths t1 t2;;

let rec aux i l = match l with
	 []->i
	|_::t -> aux(i+1)t ;;
let length = aux 0 l;;

let length l =
	let rec aux i l = match l with
	 []->i
	|_::t -> aux(i+1)t 
	in aux 0 l;; 

let rec fact = function
	0->1
	| n -> n * fact(n-1)

let rec fact n = 
	let rec aux p i = 
	if i = 0 then p
	else aux (p*i)(i-1)
	in aux 1 n;;

let fact n =
		let rec aux i f =
		if i = n then f
		else aux (i+1) (f*(1+1))
		in aux 0 1;; 


let rec fib n =
	if n<=1 then n
	else fib(n-1) + fib(n-2)


let fib n =
	if n  = 0 then 0 else
	let rec aux i u p =
	if i=n then u else
	aux(i+1)(u+p)(u)
	in aux 1 1 0;; 

let rec lmax = function
	| [] -> raise(Failure("lmax"))
	| h::[] -> h
	| h::t -> let m = lmax t in if h>= m then h else m;;

let rec lmax = function
	| [] -> raise(Failure("lmax"))
	| h::[] -> h
	| h::t -> max h (lmax t);;

	let lmax = function
		[] -> raise(Failure("lmax"))
		| h::t -> let rec aux m = function
							[] -> m 
							| h::t -> aux (max m h) t
							in aux h t;

let rec lmax = (function
| [] -> raise(Failure("lmax"))
| h::[] -> h
| h1::h2::t -> lmax (max h1 h2 :: t);;





