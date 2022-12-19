
open Context;;
open Lib;;

type arith_oper =
    Opp;;

let get_ar_op=function
Opp->Float.neg;;

type arith_bi_oper =
    Sum | Sub | Prod | Div | Mod | Pow;;

let get_bi_op= function 
    Sum->(+.) 
    | Sub->(-.)
    | Prod->( *.) 
    | Div->(/.)  
    |Mod->(Float.rem) 
    | Pow->( Float.pow);;


type arith =
    Float of float
  | Var of string
  | Arith_op of arith_oper * arith
  | Arith_bi_op of arith_bi_oper * arith * arith
  | Fun_call of string * arith;;

let rec eval ctx = function
    Float f ->f
  | Var name -> get_binding ctx name
  | Arith_op (op,e)-> (get_ar_op op) (eval ctx e)
  |Arith_bi_op (op,e1,e2)-> (get_bi_op op) (eval ctx e1) (eval ctx e2)
  |Fun_call (name,e)-> (get_function name) (eval ctx e);;
