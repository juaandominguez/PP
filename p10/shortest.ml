let rec jump (i,j) d=List.init d (fun k->(i,j+k+1))@
                      List.init d (fun k->(i+k+1,j))@
                      List.init d (fun k->(i-k-1,j))@
                      List.init d (fun k->(i,j-k-1));;

let valid (i,j) d tree reco=
  List.filter (fun x->List.mem x tree && not (List.mem x reco)) (jump (i,j) d);;

let trees =
  [(1,1); (1,3); (1,6); (1,11); (1,12); (1,15); (1,16); (2,1); (2,15); (3,6);
  (3,7); (3,9); (3,12); (4,3); (4,12); (4,15); (5,1); (6,3); (6,7); (6,9);
  (6,13); (6,14); (6,16); (7,3); (7,5); (7,16); (8,10); (8,11); (8,13);
  (8,16); (9,1); (9,3); (9,13); (10,6); (10,9); (10,10); (11,16); (12,1);
  (12,4); (12,6); (12,13); (13,11); (13,13); (14,1); (14,7); (14,9); (15,2);
  (15,4); (15,6); (15,7); (15,13); (15,16); (16,4); (16,11); (16,13);
  (16,16)];;

let shortest_tour m n tree salt=
let rec expand=function
      ([], _, _, []) -> raise Not_found
      | ([], _ , visitados, nivel_sig) ->
           expand (nivel_sig, valid (List.hd (List.hd nivel_sig)) salt tree visitados, visitados, [])
      |([h],[],visitados,nivel_sig)-> expand([],[],visitados,nivel_sig)
      |(h::t,[],visitados,nivel_sig) ->
          expand(t,valid (List.hd (List.hd t)) salt tree visitados,visitados,nivel_sig)
      | (h::t, h1::t1, visitados, nivel_sig) ->
        if h1 = (m, n)
        then List.rev (h1::h)
        else expand (h::t, t1 , h1::visitados, (h1::h)::nivel_sig)
  in expand ([[(1,1)]], valid (1,1) salt tree [(1,1)], [(1,1)], []);;