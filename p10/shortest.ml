let rec jumps (i,j) d=List.init d (fun k->(i,j+k+1))@
                      List.init d (fun k->(i+k+1,j))@
                      List.init d (fun k->(i-k-1,j))@
                      List.init d (fun k->(i,j-k-1));;

let valid (i,j) d tr reco=
  List.filter (fun x->List.mem x tr && not (List.mem x reco)) (jumps (i,j) d);;

let shortest_tour m n tree d=
let rec aux = function
  ([], _, _, []) -> raise Not_found
  | ([], _ , visited, next) -> aux (next, valid (List.hd (List.hd next)) d tree visited, visited, [])
  |([h],[],visited,next)-> aux([],[],visited,next)
  |(h::t,[],visited,next) -> aux(t,valid (List.hd (List.hd t)) d tree visited,visited,next)
  | (h::t, h1::t1, visited, next) -> if h1 = (m, n) then List.rev (h1::h)
      else aux (h::t, t1 , h1::visited, (h1::h)::next)
  in aux ([[(1,1)]], valid (1,1) d tree [(1,1)], [(1,1)], []);;
