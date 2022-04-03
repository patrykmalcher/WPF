(* Zadanie 5 *)
(* Patryk Malcher *)

(* wyjątek podnoszony, gdy graf z wejścia jest cykliczny *)
exception Cykliczne

(* ustawia wartość mapy m[x] = y *)
let set_map_val m x y = PMap.add x y m

(* zwraca stopień wierzchołka x *)
let get_deg m x = 
    if PMap.mem x m then PMap.find x m
    else 0

(* zwraca listę sąsiedztwa wierzchołka x *)
let get_nbrs m x = 
    if PMap.mem x m then PMap.find x m
    else []

(* zwraca czy graf jest DAGiem, czy posiada cykl *)
let has_cycle m = 
    let ans = ref false in 
        let f = fun a b -> if b > 0 then ans := true; in 
            PMap.iter f m;
        !ans

(* zwraca wierzchołki posortowane topologicznie *)
let topol lst = 
    (* tworzy nowy graf i tablicę stopni wierzchołków *)
    let graph = ref PMap.empty and degs_map = ref PMap.empty in
    (* tworzy graf z listy na wejściu *)
    let f = fun (v, elist) -> 
        let nbrs_ref = ref (get_nbrs !graph v) in
        begin
            List.iter (fun w -> begin degs_map := set_map_val !degs_map w (get_deg !degs_map w + 1); nbrs_ref := w::!nbrs_ref; end) elist; 
            graph := set_map_val !graph v !nbrs_ref;
        end
    in List.iter f lst;
    (* tworzy kolejkę z wierzchołkami, które są już przetworzone (mają stopień wchodzący == 0) *)
    let zero_indegs = ref [] in 
    (* wypełnia powyższą kolejkę *)
    let get_zeroes = fun (v, _) -> if (get_deg !degs_map v) = 0 then begin zero_indegs := v::!zero_indegs; degs_map := set_map_val !degs_map v (-1); end
    in List.iter get_zeroes lst;
    (* tworzy listę wynikową *)
    let ans = ref [] in
    (* sortuje topologicznie *)
    while (!zero_indegs <> [])
    do
        let front = List.hd (!zero_indegs) in (* get front *)
        begin
            zero_indegs := List.tl (!zero_indegs); (* remove front *)
            ans := front::!ans;
            let neighbours = get_nbrs !graph front in 
                let f = fun v -> 
                    begin
                        if (get_deg !degs_map v) = 1 then zero_indegs := v::!zero_indegs;
                        degs_map := set_map_val !degs_map v (get_deg !degs_map v - 1);
                    end
                in List.iter f neighbours;
        end
    done;
    (* podnosi wyjątek, gdy graf jest cyklem *)
    if (has_cycle !degs_map) then raise Cykliczne
    else List.rev !ans
