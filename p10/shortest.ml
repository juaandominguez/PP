
let trees =
  [(1,1); (1,3); (1,6); (1,11); (1,12); (1,15); (1,16); (2,1); (2,15); (3,6);
  (3,7); (3,9); (3,12); (4,3); (4,12); (4,15); (5,1); (6,3); (6,7); (6,9);
  (6,13); (6,14); (6,16); (7,3); (7,5); (7,16); (8,10); (8,11); (8,13);
  (8,16); (9,1); (9,3); (9,13); (10,6); (10,9); (10,10); (11,16); (12,1);
  (12,4); (12,6); (12,13); (13,11); (13,13); (14,1); (14,7); (14,9); (15,2);
  (15,4); (15,6); (15,7); (15,13); (15,16); (16,4); (16,11); (16,13);
  (16,16)];;


let jumps (i,j) d = List.init d (fun k-> i,j+k+1)@
					List.init d (fun k-> i,j+k-1)@
					List.init d (fun k-> i-k+1,j)@
					List.init d (fun k-> i-k-1,j);;

let filtrar (i,j) = 
	List.filter(fun x -> List.mem x l2) l1;;

let rec tour x y l m=
	let rec aux last (i,j) way trees = 
	if (i,j)=(x,y) then way else
	if trees = [] then raise Not_found else
	let head = (List.hd trees) in
	if List.mem (i,j) (jumps(i,j) m) && List.mem (i,j) trees then aux (i,j) head (head::way) (trees)
	else aux last (i,j) (if way = [] then [] else (List.tl way)) (List.tl trees)
in aux (1,1) (1,1) [] l;;

let rec tour2 x y l d =
	let rec complete path saltos = match saltos with
	| [] -> raise Not_found
	| h::t -> if h =  (x,y) then List.rev path
	else try complete (h::path) valid (h d path) with
		Not_found -> complete path t 
in if List.mem (1,1), l then complete [] [(1,1)]
	else raise Not_found;;


  