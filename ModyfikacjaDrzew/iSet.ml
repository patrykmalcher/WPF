(*Autor: Patryk Malcher*)
(*w parze z Jakub Dawidowicz*)
(*Zadanie 3*)

(*drzewo = (lewe poddrzewo, prawe poddrzewo, klucz = przedzial (a, b), wysokosc drzewa, ilosc elementow w poddrzewach)*)
type t = 
    | Node of t * t * (int * int) * int * int
    | Empty

(*puste drzewo*)
let empty = Empty

(*bezpieczna suma dwoch liczb calkowitych nie przekracza min_int i max_int*)
let sum a b = 
    if a < 0 && b < 0 && a < min_int - b then min_int
    else if a > 0 && b > 0 && a > max_int - b then max_int
    else a + b 

(*sprawdza czy drzewo jest puste*)
let is_empty = function 
    | set -> set = empty 

(*ilosc elementow w kluczu T*)
let count = function 
    | Node(_, _, (a, b), _, _) -> 
        if a = min_int then sum (sum (sum b (max_int)) (1)) (1)
        else sum (sum b (-a)) (1) 
    | _ -> 0

(*wysokosc drzewa*)
let height = function 
    | Node(_, _, _, h, _) -> h 
    | _ -> 0

(*ilosc elementow w poddrzewach T*)
let subtree = function 
    | Node(_, _, _, _, x) -> x
    | _ -> 0

(*tworzy nowe drzewo z poddrzewami l, r i kluczem k*)
let create l r k =  Node(l, r, k, max (height l) (height r) + 1, sum (sum (count l) (subtree l)) (sum (count r) (subtree r)))

(*porownuje polozenie dwoch przedzialow wzgledem siebie*)
let cmp (x1, y1) (x2, y2) = 
    if y1 < x2 then -1 
    else if y2 < x1 then 1
    else 0

(*porownuje polozenie dwoch przedzialow przedluzonych o 1 element wzgledem siebie*)
let cmp2 (x1, y1) (x2, y2) = 
    if sum y1 1 < x2 then -1
    else if sum y2 1 < x1 then 1
    else 0

(*sprawdza czy element x jest w drzewie*)
let mem x s = 
    let rec mem_help = function 
        | Node(l, r, k, _, _) ->
            let help = cmp (x, x) k
            in 
                help = 0 || (if help < 0 then mem_help l else mem_help r) 
        | _ -> false
    in 
        mem_help s 

(*wywołuje f na kazdym przedziale w drzewie*)
let iter f s = 
    let rec iter_help = function 
        | Node(l, r, k, _, _) -> iter_help l; f k; iter_help r;
        | _ -> () 
    in 
        iter_help s

(*  zwraca [(f xN ... (f x2 (f x1 a))...)] *)
(*  x'y to przedzialy uporzadkowane rosnaca *)
let rec fold f s a = 
    match s with 
        | Node(l, r, k, _, _) -> fold (f) (r) (f (k) (fold (f) (l) (a)))
        | _ -> a 
    
(*zwraca liste wszystkich przedzialow w drzewie*)
let elements s = 
    let help = fold (fun x acc -> x::acc) s [] 
    in 
        List.rev help 

(*zwraca ilosc elementow <= x w drzewie*)
let rec below x s = 
    match s with 
        | Node(l, r, (a, b), _, _) -> 
            let help = cmp (x, x) (a, b)
            in 
                if help = 0 then 
					if a = min_int then sum (sum (sum (sum (sum (x) (subtree l)) (1)) (max_int)) (count l)) 1
					else sum (sum (sum (sum (x) (-a)) (1)) (subtree l)) (count l) 
                else if help = -1 then below x l 
                else 
					if a = min_int then sum (sum (sum (below x r) (sum (subtree l) (sum (sum (b) (max_int)) (1)))) (count l)) 1
					else sum (sum (below x r) (sum (subtree l) (sum (sum (b) (-a)) (1)))) (count l)
        | _ -> 0 

(*balansuje drzewo (zmodyfikowana funkcja z danego kodu)*)
let bal l r k =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then
	match l with
	    | Node (ll, lr, lk, _, _) ->
		if height ll >= height lr then create ll (create lr r k) lk
		else
		(match lr with
		    | Node (lrl, lrr, lrk, _, _) ->
			create (create ll lrl lk) (create lrr r k) lrk
		    | Empty -> assert false)
	    | Empty -> assert false
    else if hr > hl + 2 then
	match r with
	    | Node (rl, rr, rk, _, _) ->
		if height rr >= height rl then create (create l rl k) rr rk
		else
		(match rl with
		    | Node (rll, rlr, rlk, _, _) ->
			create (create l rll k) (create rlr rr rk) rlk
		    | Empty -> assert false)
	    | Empty -> assert false
	else create l r k

(*dodaje przedzial rozlaczny z reszta przedzialow w drzewie do drzewa*)
let rec addS x s = 
    match s with 
        | Node(l, r, k, _, _) ->
            let help = cmp x k 
            in 
                if help = 0 then assert false 
                else if help = -1 then bal (addS x l) (r) (k)
                else bal (l) (addS x r) (k)
        | _ -> Node(Empty, Empty, x, 1, 0)

(*laczy dwa rozlaczne zbiory oraz przedzial rozlaczny z tymi zbiorami*)
let rec joinS l r k = 
    match (l, r) with 
        | (Empty, _) -> addS k r
        | (_, Empty) -> addS k l 
        | (Node(ll, lr, lk, lh, _), Node(rl, rr, rk, rh, _)) -> 
            if lh > rh + 2 then bal ll (joinS lr r k) lk
	        else if rh > lh + 2 then bal (joinS l rl k) rr rk
	        else create l r k

(*laczy dwa drzewa*)
let rec connect a b = 
    match (a, b) with 
        | (Empty, b) -> b 
        | (a, Empty) -> a 
        | (Node(al, ar, ak, ah, _), Node(bl, br, bk, bh, _)) -> 
            if ah > bh + 2 then bal (al) (connect (ar) (b)) (ak)
            else bal (connect (a) (bl)) (br) (bk)

(*zwraca trojke (l,p,r) *)
(*gdzie l - zbior elementow < x, p - czy x jest w drzewie, r - zbior > x *)
let rec split x = function 
    | Node(l, r, (a, b), _, _) ->
        let help = cmp (x, x) (a, b)
        in 
            if help = 0 then 
                ((if x = a then l else addS (a, sum (x) (-1)) l), true, (if x = b then r else addS (sum (x) (1), b) r))
            else if help = -1 then 
                let (a1, b1, c1) = split x l 
                in 
                    (a1, b1, joinS (c1) (r) ((a, b)))
            else 
                let (a1, b1, c1) = split x r 
                in 
                    (joinS (l) (a1) ((a, b)), b1, c1)
    | _ -> (Empty, false, Empty)

(*usuwa przedzial z drzewa*)
let remove (a, b) s = 
    let (l, _, _) = split a s and (_, _, r) = split b s 
    in 
        connect l r

(*zwraca sume wszystkich przedzialow z niepustym przecieciem z x*)
let rec merge x = function 
    | Node(l, r, k, _, _) ->
        let help = cmp2 x k 
        in 
            if help = 0 then
                let (a, _) = merge x l and (_, b) = merge x r and (xp, xk) = x and (kp, kk) = k
                in
                    (min a (min xp kp), max b (max xk kk))
            else if help = -1 then 
                merge x l 
            else 
                merge x r
    | _ -> x

(*dodaje przedzial x do drzewa*)
let add x s = 
    let (a, b) = merge x s 
    in
        let (x1, _, _) = split a s 
        and (_, _, y1) = split b s 
        in 
            joinS x1 y1 (a, b)
