open List

exception Not_found

(* Función para obtener los vecinos de una casilla dada *)
let get_neighbors (x, y) m n jump trees =
  (* Generamos todas las posibles casillas vecinas a distancia n o menos de (x, y) *)
  let candidates =
    fold_left (fun acc i -> acc @ [(x + i, y); (x, y + i)]) [] (range 1 jump)
  in
  (* Filtramos las casillas que estén fuera del tablero o que no sean árboles *)
  filter (fun (x, y) -> x >= 1 && x <= m && y >= 1 && y <= n && mem (x, y) trees) candidates

(* Función para encontrar un camino desde (1, 1) hasta (m, n) saltando de árbol en árbol *)
let tour m n trees jump =
  (* Inicializamos la cola de nodos por visitar con la posición inicial (1, 1) *)
  let queue = [(1, 1)] in
  (* Inicializamos el conjunto de casillas visitadas con la posición inicial (1, 1) *)
  let visited = [(1, 1)] in
  (* Mientras queden nodos por visitar en la cola... *)
  while queue <> [] do
    (* Sacamos el primer nodo de la cola y lo expandimos *)
    let current = hd queue in
    queue <- tl queue;
    (* Obtenemos los vecinos de la casilla actual a distancia n o menos *)
    let neighbors = get_neighbors current m n jump trees in
    (* Para cada vecino... *)
    iter (fun neighbor ->
      (* Si es la posición final (m, n), devolvemos el camino como una lista de casillas *)
      if neighbor = (m, n) then
        raise (Found (current :: neighbor :: visited))
      (* Si no ha sido visitado previamente, lo agregamos a la cola y al conjunto de casillas visitadas *)
      else if not (mem neighbor visited) then begin
        queue <- queue @ [neighbor];
        visited <- neighbor :: visited
      end
    ) neighbors
  done;
