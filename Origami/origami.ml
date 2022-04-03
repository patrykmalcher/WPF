(* Patryk Malcher *)
(* Zadanie 4 *)

(* epsilon - koryguje niedokładności *)
let e = 10e-12

(* reprezentacja punktu *)
type point = float * float

(* reprezentacja kartki *)
type kartka = point -> int

(* zamienia wartość logiczną na inta *)
let to_int = function 
    | true -> 1
    | false -> 0

(* zwraca prostokątną kartkę *)
let prostokat (x1, y1) (x2, y2) = function 
    | (a, b) -> to_int (x1 <= a && a <= x2 && y1 <= b && b <= y2)

(* zwraca okrągłą kartkę *)
let kolko (x, y) p = 
    let square d = d *. d 
    in
        function 
            | (a, b) -> to_int (sqrt (square (a -. x) +. square (b -. y)) <= p +. e)

(* zwraca położenie punktu względem prostej, -1 = prawo, 0 = na prostej, 1 = lewo *)
let side (x1, y1) (x2, y2) (x, y) = 
	let sign f = if f -. e <= 0. && 0. <= f +. e then 0 else if f > 0. then 1 else -1
	and value = (x2 -. x1) *. (y -. y1) -. (x -. x1) *. (y2 -. y1) 
	in 
		sign value 

(* zwraca punkt symetryczny względem prostej *)
let symmetrical (x1, y1) (x2, y2) (x, y) = 
	if x1 = x2 then (x1 *. 2. -. x, y)
	else if y1 = y2 then (x, y1 *. 2. -. y)
	else let a = (y1 -. y2) /. (x1 -. x2) and ap = (x2 -. x1) /. (y1 -. y2) 
	in 
		let b = y2 -. a *. x2 and bp = y -. ap *. x
		in 
			let xc = (bp -. b) /. (a -. ap)
			in
		    	let yc = a *. xc +. b
				in 
					(2. *. xc -. x, 2. *. yc -. y)

(* składa kartkę *)
let zloz p1 p2 k =
	function p ->	
		let position = side p1 p2 p
		and pp = symmetrical p1 p2 p
		in 
			if position = -1 then 0
			else if position = 0 then k p 
			else k p + k pp

(* składa kartkę kolejno wzdłuż wszystkich prostych z listy *)
let skladaj l k = List.fold_left (fun acc (p1, p2) -> zloz p1 p2 acc) k l	
