(*Patryk Malcher w grupie z Bartek Kucypera*)
 
type 'a queue =                                   (*drzewo = (lewe_poddrzewo, prawe_poddrzewo, wartość w węźle, wysokość) | Null*)
    | Node of 'a queue * 'a queue * 'a * int 
    | Null;;

let empty = Null;;

let rec join x y =
    match (x, y) with
    | (_, Null) -> x
    | (Null, _) -> y
    | (Node(a1, a2, a3, a4), Node(b1, b2, b3, b4)) ->
        if a3 > b3 then join y x
        else let a2 = join (Node(b1, b2, b3, b4)) (a2) in 
        let a1, a2 = if a1 = Null then a2, a1 else a1, a2 in 
        let a4 = if a2 = Null then 1 else a4 in 
        if a2 = Null then Node(a1, a2, a3, a4) 
        else match (a1, a2) with  
            | (Node(a11, a12, a13, a14), Node(a21, a22, a23, a24)) -> 
                let a1, a2 = if a24 > a14 then a2, a1 else a1, a2 in 
                let a4 = min (a14) (a24) + 1 in
                Node(a1, a2, a3, a4)
            | (_, _) -> Null;;    

let add a b = join (b) (Node(Null, Null, a, 0));;
 
exception Empty;;

let delete_min = function
    | Null -> raise (Empty)
    | Node(a, b, c, d) -> (c, join (a) (b));;

let is_empty = function 
    | Null -> true 
    | _ -> false;;
