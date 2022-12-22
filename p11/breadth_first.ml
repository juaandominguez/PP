
open G_tree;;

let rec breadth_first = function
    Gt (x, []) -> [x]
  | Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;

let rec breadth_first_t t =
  let rec aux acc curr next = 
      match (curr,next) with
      | [],[] -> List.rev acc
      | [],next -> aux acc (List.rev next) []
      | Gt(x,t2)::t1,next -> aux (x::acc) t1 (List.rev_append t2 next)
    in aux [] [t] [];;

let t2 = Gt(0,List.init 300_000 (fun x -> Gt(x+1,[])));;

