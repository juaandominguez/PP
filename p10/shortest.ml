let jumps (i,j) d = List.init d (fun k-> i,j+k+1)@
					List.init d (fun k-> i,j-k-1)@
					List.init d (fun k-> i+k+1,j)@
					List.init d (fun k-> i-k-1,j);;

let valid (i,j) d tree reco=
  List.filter (fun x->List.mem x tree && not (List.mem x reco)) (jumps (i,j) d);;

let shortest_tour m n tree d=
let rec expand = function
      ([], _, _, []) -> raise Not_found
      | ([], _ , visitados, nivel_sig) ->
           expand (nivel_sig, valid (List.hd (List.hd nivel_sig)) d tree visitados, visitados, [])
      |([h],[],visitados,nivel_sig)-> expand([],[],visitados,nivel_sig)
      |(h::t,[],visitados,nivel_sig) ->
          expand(t,valid (List.hd (List.hd t)) d tree visitados,visitados,nivel_sig)
      | (h::t, h1::t1, visitados, nivel_sig) ->
        if h1 = (m, n)
        then List.rev (h1::h)
        else expand (h::t, t1 , h1::visitados, (h1::h)::nivel_sig)
  in expand ([[(1,1)]], valid (1,1) d tree [(1,1)], [(1,1)], []);;