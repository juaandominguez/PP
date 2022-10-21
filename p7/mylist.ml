let hd l = match l with
  [] -> raise(Failure("hd"));
| h::t -> h;;

let tl l = match l with
  [] -> raise(Failure("tl"));
| h::t -> t;;

let rec find f l = match l with
  [] -> raise(Failure("Not_found"))
| h::t when not(f h) -> find f t
| h::t when (f h) -> h;;