let curry f x y = f (x, y);;
let uncurry g (x, y) = g x y;;
uncurry (+);;
(*- : int * int -> int = <fun>*)
let sum = (uncurry(+));;
(*val sum : int * int -> int = <fun>*)
(*sum 1;;*)
(*Error de tipos: Esperado int * int*)
sum (2,1);;
(*- : int = 3*)
let g = curry (function p -> 2 * fst p + 3 * snd p);;
(*val g : int -> int -> int = <fun>*)
(*g (2,5);;*)
(*Error de tipos: Esperado int*)
let h = g 2;;
(*val h : int -> int = <fun>*)
h 1, h 2, h 3;;
(*- : int * int * int = (7, 10, 13)*)
let comp f g x =
  f (g x);;
(*val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun>*)
let f = let square x = x * x in comp square ((+) 1);;
(*val f : int -> int = <fun>*)
f 1, f 2, f 3;;
(*- : int * int * int = (4, 9, 16)*)
let i a = a;;
let j (a,b) = a;;
let k (a,b) = b;;
let l a = a::[];;
(*Se puede escribir una única función ya que a pesar de
cambiar 'a' o 'b' seguirá siendo la misma función*)