open List;;

let trees =
  [(1,1); (1,3); (1,6); (1,11); (1,12); (1,15); (1,16); (2,1); (2,15); (3,6);
  (3,7); (3,9); (3,12); (4,3); (4,12); (4,15); (5,1); (6,3); (6,7); (6,9);
  (6,13); (6,14); (6,16); (7,3); (7,5); (7,16); (8,10); (8,11); (8,13);
  (8,16); (9,1); (9,3); (9,13); (10,6); (10,9); (10,10); (11,16); (12,1);
  (12,4); (12,6); (12,13); (13,11); (13,13); (14,1); (14,7); (14,9); (15,2);
  (15,4); (15,6); (15,7); (15,13); (15,16); (16,4); (16,11); (16,13);
  (16,16)];;

let canJump (i, j) visited t distance =
  filter (fun x -> x<>(i,j) && not (mem x visited) && ((abs (fst x - i) < distance) || (abs (snd x - j) < distance))) t;;
   
  ( 
let shortest_tour x y trees distance =
  let rec aux current visited next = match (hd current) with
  | [] -> if next = [] then raise Not_found else aux next visited []
  | h::_ -> 
