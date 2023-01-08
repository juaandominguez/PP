(*16/09/2022*)
let alltrue = function x -> true;;
let alltruetyped = function (x:bool)-> true;;
let alltrue x = true;;
let alltrue _ = true;; (*Better*)
let x = 1 in 2 * 3;;
let id = function x -> x;;
let not_zero = function 0 -> false | _ -> true;;
(*21/09/2022*)
(* |> pipe *)
(* match <e> with p1->e1 ...
let f = function x->
				 (function y ->x+y);;
let x y = x+y;; *)
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

2+1; "hola" ^ "adios";; (*Should have type unit but ok*)

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

(*LISTAS*)
[1;2;3];;
[1;100];;
['a';'e';'i'];;
let l = [1;2;3];; (*int list*)
List.length l ;;
List.hd;;
List.tl;;
List.hd (List.tl (List.tl l));;
(* let rec length l =
	if l = [] then 0 else
	1 + length (List.tl l);;
	<a'>::<a' list> *)

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

let rec lmax = function
| [] -> raise(Failure("lmax"))
| h::[] -> h
| h1::h2::t -> lmax (max h1 h2 :: t);;

let rec for_all f = function
| [] -> true;
| h::t -> f h && for_all f t;;

let for_all p l = List.fold_left (fun b h -> b && p h ) true l;; (*Mejor la primera*)

let rec sorted = function
| [] | _::[]-> true;
| h1::h2::t -> h1<=h2 && sorted (h2::t);;

let rec insert x = function
[] -> [x]
| h::t -> if x<= h then x::h::t else h::insert x t;;

let rec isort = function
| [] -> [];
| h::t -> insert h (isort t);;

let insert' x l=
let rec aux p1= function
[]-> List.rev (x::p1)
|h::t -> if x <= h then List.rev_append p1 (x::h::t)
else aux (h::p1) t
in aux [] l;;

let isort' ord l =
		let rec aux res = function
		| [] -> res
		| h::t -> aux (insert' h ord) t
	in aux [] l;;

let rec divide = function
| h1::h2::t -> let t1,t2 = divide t in h1::t1,h2::t2
| l -> l,[];;

let rec merge = function
| ([],l) | (l,[]) -> l
| (h1::t1, h2::t2) ->  if h1<= h2 then h1 :: merge (t1, h2::t2)
else h2 :: (merge (h1::t1) t2);;

let rec msort = (function
| [] -> []
| [x] -> [x]
| l -> let l1,l2 = divide l in merge (msort l1, msort l2));;

	let threatens (i1,j1) (i2,j2) = 
			i1 = i2 || 
			j1 = j2 ||
			abs(j2-j1) = abs(i2-i1);;
			
	(* let rec compatible p = function
	[] -> true;
	| h::t -> not(threatens p h) && compatible p t;; *)

	let compatible p l = not (List.exists (threatens p l) s);; 

	let rec queens n =
		let rec complete path (i,j) =
			if i>n then Some path
			else if j>n then None
			else if compatible(i,j) path then match complete((i,j)::path) (i+1,1) with
				None -> complete path(i,j+1)
				| sol -> sol
			else complete path (i,j+1)
		in queens [] (1,1);;


	let rec queens n =
		let rec complete path (i,j) =
			if i>n then Some path
			else if j>n then None
			else if compatible(i,j) path then match complete((i,j)::path) (i+1,1) with
				None -> complete path(i,j+1)
				| sol -> sol
			else complete path (i,j+1)
		in queens [] (1,1);;



let printSol = function
	[] -> print_newline []
| (_,j)::[] -> print_int j : print_newline []
| (_,j)::t -> print_int j : print_char 's'; printSol t;



j+1


print_all_queens (int_of_string(Sys.argv.(1)));;


(*16/11/22*)

type 'a option =
		None
	| Some of 'a;;


type mayBeAnInt =
		AnInt of int
	| NotAnInt;;


let quo x y = match x,y  with
		_, AnInt 0 -> NotAnInt
	| AnInt m, AnInt n -> AnInt (m/n)
	| _ ->NotAnInt;;

type foo = Foo;;

type foo2 = Foo1 | Foo2;;

type palo = Trebol | Picas | Corazones | Diamantes;;

type boolean = F|T;;

let verdadero = T;;

let falso = F;;

let conj b1 b2 = match b1,b2 with 
		T,T -> T
	| _ -> F;;

let (&&&) = conj;;


let conj b1 b2 = match b1,b2 with 
		f,_|_,f -> falso
	| _ -> verdadero;;

type otherint = I of int;;

type num = I of int | F of float;;

type dobleint = L of int | R of int;;

type nat = Z | S of nat;;

let uno = S Z;;

let dos = S UNO;;

let rec sum n1 = function
		Z -> n1
	| S n2 -> sum (S n1) n2;;

let rec nat_of_int = function
		0 -> Z
	| n -> S nat_of_int (n-1);;


type 'a btree =
	E | N of 'a * 'a btree * 'a btree;;

type 'a list =
		[] 
	| (::) of 'a * 'a list;;

let h x = N (x,E,E);; (*E representa el árbol vacío*)

let left_branch ((N,_,_)) = l;;

let rec nnodes = function
		E -> 0 
	| N(_,i,d) -> 1+ nnodes i + nnodes d;;


let rec height = function
	| E -> 0
	| N(_,i,d) -> 1 + max (height i) (height d);;

let rec inorder = function
	| E -> []
	| N(r,lb,rb) -> inorder lb @ [r] @ inorder rb;; 


let rec leaves = 
	| E -> []
	| N (r,E,E) -> [r]
	| N (_,lb,rb) -> leaves lb @ leaves rb;;

type 'a st_tree = (*Full bst*)
		Leaf of 'a
	| Node of 'a * 'a st_tree * 'a st_tree;;

let rec mirror = function
| Leaf x -> Leaf x
| Node (v,l,r) -> Node (v, mirror r, mirror l);;

let rec btree_of_st_tree = function
| Leaf x -> N(x,E,E)
| Node (v,l,r) -> N(v,btree_of_st_tree l, btree_of_st_tree r);;

let rec st_tree_of_btree = function
| E -> raise (Invalid_argument"st_tree_of_btree");
| N (x,E,E) -> Leaf x
| N (x,l,r) -> Node(x,st_tree_of_btree l,  st_tree_of_btree r);;

type 'a gtree =
GT of 'a * 'a gtree list;

let rec nngtree = function
| GT(_,h::t) -> List.fold_left (+) 1 (List.map nngtree l) ;;

let rec nngtree (GT,(_,l)) =
	List.fold_left (+) 1 (List.map nngtree l);;

let rec nngtree = function
| GT (_,[]) -> 1
| GT (x,h::t) -> nngtree h + nngtree (GT (x,t));;

let rec mirror (GT, (v,l)) =
	GT(v,List.rev(List.map mirror l));;

output_char;;
- : out_channel -> char -> unit = <fun>

stdout;;
- : out_channel = <abstr>

output_char stdout 'a';;
a- : unit = ()

let print_char c = output_char stdout c;;
val print_char : char -> unit = <fun>

let _ = print_char 'x' in print_char 'y';;
xy- : unit = ()

let totrue _ = true;;
val totrue : 'a -> bool = <fun>

totrue (print_char 'A' ) && totrue (print_char 'B');;
AB- : bool = true

"a" ^ "b"; 2 * 3;;
let ignore _ = ();;

String.get;; = str.[n];;
- : string -> int -> char = <fun>

let output_string c s = 
	let n = String.length s in
	let rec loop i =
		if i>=n then ()
		else begin output_char c s.[i]; loop (i+1) end
	in loop 0;;
val output_string : out_channel -> string -> unit = <fun>

let print_string s = output_string stdout s;;
val print_string : string -> unit = <fun>

let print_endline s = 
	print_string (s ^ "\n");
	flush stdout;;
val print_endline : string -> unit = <fun>
stdout;; stderr;;
- : out_channel = <abstr>

open_out;;
- : string -> out_channel = <fun>

let sal = open_out "Prueba.md";;
val sal : out_channel = <abstr>

flush sal;;
- : unit = ()

close_out sal;;
- : unit = ()

input_char;;
- : in_channel -> char = <fun>

open_in;;
- : string -> in_channel = <fun>

let read_char () = input_char stdin;;
read_line;; 

let rec output_string_list out = function
| [] -> ()
| h::t -> output_string out (h^"\n"); output_string_list out t;;
(*output_string_list: out_channel -> string list -> unit *)

let rec input_string_list input =
	
	try let x = input_line input in x :: input_string_list input 
	with End_of_file -> [];;

let rec iter f = function
| [] -> ()
| h::t -> f h; iter f t;;

let output_string_list out l = iter (fun s -> output_string out (s ^ "\n")) l;;

pos_in;;
- : in_channel -> int = <fun>

seek_in;;(*Cambia puntero a posicion indicada*)
- : in_channel -> int -> unit = <fun>

seek_out;;
- : out_channel -> int -> unit = <fun>
output_value;;
- : out_channel -> 'a -> unit = <fun>
ref;;
- : 'a -> 'a ref = <fun>
(!);;
- : 'a ref -> 'a = <fun>
let i = ref 0;;
val i : int ref = {contents = 0}
!i;;
(:=);;
- : 'a ref -> 'a -> unit = <fun>

let fact n =
	let p = ref 1 in
	for i = 1 to n do 
		p:= !p * i
	done;
	!p;;

(*for downTo*)
let fact n=
	let f = ref 1 in
	List.iter (fun i -> f:=(!f*i)) (let i1 = 1 in let i2 = n in List.init (max 0 (i2-i1+1)) ((+)i1));
	!f;;

(* while <b> do <e> done*)
let fact n =
	let p = ref n in
	let i = ref (n-1) in
	while !i>1 do p:=((!p)*(!i)); i := ((!i)-1); done;
	!p;;

let n = ref 0;;

let turno () =
	 n := !n+1;
	 !n;;
	 val turno : unit -> int = <fun>

let turno = 
	let n = ref 0 in 
	n:= !n+1;
	!n;;

let turno = let n = ref 0 in function () -> 
	n:= !n +1;
	!n;;

let reset () =
	n := 0;;

let turno,reset =
  let n = ref 0 in 
	(function ()-> n:=!n+1;!n),
	(function ()-> n:=0);;

module Counter : 
sig
	val turno: unit -> int
	val reset: unit -> unit
end =
struct
	let n = ref 0
	let turno () =
	 n := !n+1;
	 !n
	let reset () = 
	n:=0
end;;
SALIDA: module Counter : sig val turno : unit -> int val reset : unit -> unit end

module C = Counter;;
SALIDA: module C = Counter
C.turno ();;

module Counter (): 
sig
	val turno: unit -> int
	val reset: unit -> unit
end =
struct
	let n = ref 0
	let turno () =
	 n := !n+1;
	 !n
	let reset () = 
	n:=0
end;;
SALIDA: functor () -> sig val turno : unit -> int val reset : unit -> unit end

module C1 = Counter ();;
SALIDA : module C1 : sig val turno : unit -> int val reset : unit -> unit end

Sets::

module IntPair =
struct
		type t = int * int 
		let compare = Stdlib.compare
end;;
SALIDA: module IntPair : sig type t = int * int val compare : 'a -> 'a -> int end

module IPSet = Set.Make (IntPair);;

let trees = List.init 50_000 (fun _ -> 1+Random.int(500),Random.int(500)+1);;

let trees_Set = IPSet.of_list trees;;
val trees_Set : IPSet.t = <abstr>

let to_find = List.init 5_000 (fun _ -> 1+Random.int(500),Random.int(500)+1);;

let r1 = List.filter (fun p -> List.mem p trees) to_find;;

let r2 = List.filter (fun p -> IPSet.mem p trees_Set) to_find;;

let crono f x =
	let t = Sys.time () in
		x; Sys.time () -. t;;

let v = [|1;2;3|];;
val v : int array = [|1; 2; 3|]

Array.get v 0;;
- : int = 1

Array.set v 0 100;;
- : unit = ()

Sys.argv;;
- : string array = [|"/usr/bin/utop"|]

v.(0);; (*100*)

Array.make 100 0;;
- : int array =
[|0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]

Array.init 100 (fun x -> x);; (*Como List.init*)

let sProd v1 v2 = 
	if Array.length v1 <> Array.length v2 then raise(Invalid_argument("sProd"))
	else begin
	let p = ref 0.0 in
	for i = 0 to Array.length v1 do
			p:= !p +. v1.(i) *. v2.(i)
	done;
	!p
end;;

let sProd v1 v2 =
		Array.fold_left ( *. ) 0. (Array.map2 ( *. ) v1 v2);;

type persona ={nombre: string; edad: int};;
type persona = { nombre : string; edad : int; }

let thGol = {nombre = "Thgol"; edad = 95};;
val thGol : persona = {nombre = "Thgol"; edad = 95}

let mas_viejo p =
	{nombre = p.nombre;edad = p.edad + 1};;
val mas_viejo : persona -> persona = <fun>

type persona ={nombre: string; mutable edad: int};;
thGol.edad <- 30;;
- : unit = ()

let envejece p = p.edad <- p.edad + 1;;

let mas_viejo p =
	{p with edad = p.edad +1}

type counter =
	{turno: unit -> int; reset : unit -> unit};;

let c1 =
		let n = ref 0 in
		{turno = (fun () -> n:= !n+1; !n); reset = fun() -> n:= 0};;

let makeCounter () =
			let n = ref 0 in
			{turno = (fun () -> n:= !n+1; !n); reset = fun() -> n:= 0};;

type 'a ref = {mutable contents: 'a ref};;
let (!) v = v.contents;;
let (:=) v x = v.contents <- x;;

type personaplus =
	{nombre: string; edad: int; pais: string};;

let pepevox = {nombre: string, edad: 9; pais: "ESPANIA VUAMOOOOSSS"};;

let edad p = p.edad;;

let x,y = 2+1, 3*1;;

let x = 1 and y = 2 and z = 3;; (*This makes x,y,z change at the same time*)

let x,y =  y,x;;

let rec par n = 
    n = 0 || impar (n-1)
and impar n =
    n<>0 && par(n-1);;

let c1 = object
    val mutable n = 0
    method turno = n <- n + 1; n
    method reset = n <- 0
end;;
val c1 : < reset : unit; turno : int > = <obj>

c1#turno;;

c1#turno + (c1#reset; c1#turno);; (*Se evalua en cualquier orden, CUIDADO!*)

let doble c = 2 * c#turno;;

let doble_obj c = object
    method turno = 2 * c#turno
    method reset = c#reset
end;;

class counter = object
    val mutable n = 0
    method turno = n <- n + 1; n
    method reset = n <- 0
end;;
object val mutable n : int method reset : unit method turno : int end

let counter = <reset: unit; turno: int>;;

let c3 = new counter;;
val c3 : counter = <obj>

let c4 = new counter;;

class countWithSet = object
    inherit counter
    method setValue i = n <- i
end;;
SALIDA: (class countWithSet :
  object
    val mutable n : int
    method reset : unit
    method setValue : int -> unit
    method turno : int
  end)
