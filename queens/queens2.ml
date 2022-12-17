open List;;


let rec unthreatened x1 y1 = function
| (x2, y2) :: t ->
x1 <> x2 && y1 <> y2 && x2 - x1 <> y2 - y1 && x1 - y2 <> x2 - y1 &&
unthreatened x1 y1 t
| [] -> true;;

let rec iter_n2 f x y n =
if y<n then
if x=n then iter_n2 f 0 (y + 1) n else
(f x y;
iter_n2 f (x + 1) y n);;

let rec search n f queens x y =
if unthreatened x y queens then
if length queens = n - 1 then f ((x, y) :: queens) else
iter_n2 (search n f ((x, y) :: queens)) (x + 1) y n;;

