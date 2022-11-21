 let rec remove a l =
  let rec aux a l removedlist = match l with
  | [] -> List.rev_append removedlist []
  | h::t -> if h=a then List.rev_append removedlist t else aux a t (h::removedlist)
in aux a l [];;

let rec remove_all a l =
  let rec aux a l removedlist = match l with
  | [] -> List.rev_append removedlist []
  | h::t -> if h=a then aux a t removedlist else aux a t (h::removedlist)
in aux a l [];;

let rec ldif l1 l2 = match l1,l2 with
| [],_ | _,[] -> l1
| _,h::t -> ldif (remove_all h l1) t;;

let lprod l1 l2 =

  List.flatten(List.map( function x -> List.map(function y ->(x,y)) l2) l1);;



let rec divide = function
 | h1::h2::t -> let t1,t2 = divide t in h1::t1,h2::t2
 | l -> l,[];;
