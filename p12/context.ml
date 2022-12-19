
type 'a context =
  (string * 'a) list;;

exception No_binding of string;;

let empty_context = [];;

let get_binding ctx name =
  try List.assoc name ctx with
  Not_found -> raise (No_binding name);;

let rec add_binding ctx name v = 
  match ctx with
  []->[name,v]
  |(name',v')::t-> if (name=name') then (name,v)::t
  else (name',v')::add_binding t name v;;

