let trees =
[(1,1); (1,3); (1,6); (1,11); (1,12); (1,15); (1,16); (2,1); (2,15); (3,6);
(3,7); (3,9); (3,12); (4,3); (4,12); (4,15); (5,1); (6,3); (6,7); (6,9);
(6,13); (6,14); (6,16); (7,3); (7,5); (7,16); (8,10); (8,11); (8,13);
(8,16); (9,1); (9,3); (9,13); (10,6); (10,9); (10,10); (11,16); (12,1);
(12,4); (12,6); (12,13); (13,11); (13,13); (14,1); (14,7); (14,9); (15,2);
(15,4); (15,6); (15,7); (15,13); (15,16); (16,4); (16,11); (16,13);
(16,16)];;

let jumps (i,j) d = List.init d (fun k-> i,j+k+1)@
					List.init d (fun k-> i,j-k-1)@
					List.init d (fun k-> i+k+1,j)@
					List.init d (fun k-> i-k-1,j);;

let valid (i,j) d trees l2= 
	let l1 = jumps (i,j) d in  
	List.filter(fun x -> not(List.mem x l2)&&( List.mem x l1)) trees;;


let tour m n trees d =
  let rec complete path saltos = match saltos with
  | [] -> raise Not_found
  | h::t -> if h = (m,n) then List.rev (h::path) else
      try complete (h::path) (valid h d trees path) with 
      | Not_found -> complete path t 
    in if List.mem (1,1) trees then complete [] [(1,1)]
		else raise Not_found;;


  
