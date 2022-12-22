let jumps (i,j) d = List.init d (fun k-> i,j+k+1)@
					List.init d (fun k-> i,j-k-1)@
					List.init d (fun k-> i+k+1,j)@
					List.init d (fun k-> i-k-1,j);;

let valid (i,j) d tree l =
  List.filter (fun x->List.mem x tree && not (List.mem x l)) (jumps (i,j) d);;


let tour m n trees d =
  let rec complete path available = match available with
  | [] -> raise Not_found
  | h::t -> if h = (m,n) then List.rev (h::path) else
      try complete (h::path) (valid h d trees path) with 
      | Not_found -> complete path t 
    in if List.mem (1,1) trees then complete [] [(1,1)]
		else raise Not_found;;


  
