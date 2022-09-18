();;
(*- : unit*)
2 + 5 *3;;
(*- : int = 17*)
1.0;;
(*- : float = 1.0*)
(*2-2.0;;*)
(*Error de tipos. Se espera tipo int en el segundo operando*)
(*3.0+2.0;;*)
(*Error de tipos. Se espera tipo int en ambos operandos*)
5/3;;
(*- : int =1*)
5 mod 3;;
(*- : int = 2*)
3.0 *. 2.0 ** 3.0;;
(*- : float = 24*)
3.0 = float_of_int 3;;
(*- : bool = true*)
(*sqrt 4;;*)
(*Error de tipos. Se espera tipo float (val sqrt = float -> float <fun> )*)
int_of_float 2.1 + truncate (-2.9);;
(*- : int = 0*)
truncate 2.1 + truncate (-2.9);;
(*- : int =0*)
floor 2.1 +. floor (-2.9);;
(*- : float = -1.*)
(*ceil 2.1 +. ceil -2.9;;*)
(*Error léxico. Se espera un parentesis despues de ceil*)
2.0 ** 3.0 ** 2.0;;
(*- : float = 512*)
'B';;
(*- : char = 'B'*)
int_of_char 'A';;
(*- : int = 65*)
char_of_int 66;;
(*- : char = 'B'*)
Char.code 'B';;
(*- : int = 66*)
Char.chr 67;;
(*- : char = 'C'*)
'\067';;
(*- : char = 'C'*)
Char.chr (Char.code 'a' - Char.code 'A' + Char.code 'M');;
(*- : char = 'm'*)
"this is a string";;
(*- : string = "this is a string"*)
(*"1999" + "1";;*)
(*Error de tipos. Se espera tipo int en ambos operandos*)
"1999" ^ "1";;
(*- : string= "19991"*)
int_of_string "1999" + 1;;
(*- : int = 2000*)
"\064\065";;
(*- :string = @A*)
string_of_int 010;;
(*- : string = "10"*)
not true;;
(*- : bool = false*)
true && false;;
(*- : bool = false*)
true || false;;
(*- : bool = true*)
"1" < "2";;
(*- : bool = true*)
2 < 12;;
(*- : bool = true*)
"2" < "12";;
(*- : bool = true*)
"uno" < "dos";;
(*- : bool = true*)
if 3 = 4 then 0 else 4;;
(*- : int = 4*);;
if 3 = 4 then '0' else '4';;
(*- : char = '4'*)
(*if 3 = 4 then 0 else '4';;*)
(*Error de tipos. Se espera tipo int después del else*)
(if 3 < 5 then 8 else 10) + 4;;
(*- : int = 12*)
2.0 *. asin 1.0;;
(*- : float = 3.14159265358979312*)
sin (2.0 *. asin 1.0/. 2.);;
(*- : float 1.*)
function x -> 2*x;;
(*- : int -> int <fun>*)
(function x -> 2*x) (2+1);;
(*- : int = 6*);;
let x = 1;;
(*val x : int = 1*)
let y = 2;;
(*val y : int = 2*)
x-y;;
(*- : int = -1*)
let x = y in x-y;;
(*- : int = 0*)
x - y;;
(*int = -1*)
(*z;;*)
(*Error léxico. z no tiene ningún valor asignado*)
let z = x+y;;
(*val z : int = 3*)
z;;
(*- : int = 3*)
let x=5;;
(*val x : int = 5*)
z;;
(*- : int = 3*)
let y = 5 in x+y;;
(*- : int = 10*)
x+y;;
(*- : int = 7*)
let x = x+y in let y = x*y in x+y+z;;
(*- : int = 24*)
x+y+z;;
(*- : int = 10*)
int_of_float;;
(*- : float -> int = <fun>*)
float_of_int;;
(*- : int -> float = <fun>*)
int_of_char;;
(*- : char -> int = <fun>*)
char_of_int;;
(*- : int -> char = <fun>*)
abs;;
(*- : int -> int = <fun>*)
sqrt;;
(*- : float -> float = <fun>*)
truncate;;
(*- : float -> int = <fun>*)
ceil;;
(*- : float -> float = <fun>*)
floor;;
(*- : float -> float = <fun>*)
Char.code;;
(*- : char -> int = <fun>*)
Char.chr;;
(*- : int -> char = <fun>*)
int_of_string;;
(*- : string -> int = <fun>*)
string_of_int;;
(*- : int -> string = <fun>*)
String.length;;
(*- : string -> int = <fun>*)
let f =function x -> 2*x;;
(*val f : int -> int = <fun>*)
f (2+1);;
(*- : int = 6*)
f 2+1;;
(*- : int = 5*)
let n=1;;
(*val n : int = 1*)
let g x=x + n;;
(*val g : int -> = <fun>*)
g 3;;
(*- : int = 4*)
let l = function r -> let pi = 2.0*. asin 1.0 in 2.0 *. pi *. r;;
(*val l : float -> float <fun>*)
l 3.0;;
(*- : float = 18.8495559215387587*)
(*l 2;;*)
(*Error de tipos*)
(*pi;;*)
(*Error léxico. pi no tiene ningún valor asignado*)
let pi = 2.0 *. asin 1.0;;
(*val pi : float = 3.14159265358979312*)
pi;;
(*- : float = 3.14159265358979312*)
let v= function r ->pi *. r ** 2.0;;
(*val v : float -> float = <fun>*)
v 2.0;;
(*- : float = 12.5663706143591725*)