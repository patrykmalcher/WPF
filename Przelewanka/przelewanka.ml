(* Patryk Malcher *)
(* Zadanie 6 *)

(* zwraca wynik *)
let przelewanka input_array =
    (* tablica bez szklanek o zerowej pojemności *)
    let input_array_filtered = Array.of_list (List.filter (fun (x, _) -> x <> 0) (Array.to_list input_array))
    and hash_table = Hashtbl.create 1000000
    and q = Queue.create () in 
    let n = Array.length input_array_filtered in
    (* tablica pojemności szklanek *)
    let capacity = Array.init n (fun i -> fst input_array_filtered.(i)) 
    (* tablica ze stanem szklanek, który chcemy osiągnąć *)
    and desired_state = Array.init n (fun i -> snd input_array_filtered.(i)) in
    let rec nwd a b = 
        if a = 0 then b 
        else nwd (b mod a) a
    in 
    (* NWD pojemności wszystkich szklanek *)
    let nwd_capacity = Array.fold_left (fun acc x -> nwd acc x) 0 capacity in 
    (* is_ok1 - szukana ilość wody dla każdej szklanki musi być podzielna przez NWD pojemności szklanek *)
    let is_ok1 = Array.for_all (fun x -> x mod nwd_capacity = 0) desired_state 
    (* is_ok2 - musi istnieć przynajmniej jedna szklanka, która zostanie pusta lub wypełniona w całości *)
    and is_ok2 = Array.exists (fun (x, y) -> y = 0 || x = y) input_array_filtered
    (* zwraca nowy stan powstały w wyniku opróżnienia szklanki o indeksie i *)
    and empty state i = 
        let copy = Array.copy state in 
        copy.(i) <- 0;
        copy
    (* zwraca nowy stan powstały w wyniku napełnienia szklanki o indeksie i *)
    and fill state i = 
        let copy = Array.copy state in 
            copy.(i) <- capacity.(i);
            copy 
    (* zwraca nowy stan powstały w wyniku przelania wody ze szklanki o indeksie i do szklanki o indeksie j *)
    and pour state i j = 
        let copy = Array.copy state in
            if state.(i) + state.(j) >= capacity.(j) then
            (
                copy.(i) <- state.(i) + state.(j) - capacity.(j);
                copy.(j) <- capacity.(j)
            )
            else 
            (
                copy.(j) <- state.(j) + state.(i);
                copy.(i) <- 0
            );
        copy 
    (* rozpatrza nowy stan *)
    and new_state state moves = 
        if not(Hashtbl.mem hash_table state) then
        (
            Queue.push (moves, state) q;
            Hashtbl.add hash_table state moves;
        )
    in 
    (
        Queue.push (0, Array.make n 0) q;
        Hashtbl.add hash_table (Array.make n 0) 0;
        (* BFS po wszystkich stanach *)
        while ((not(Queue.is_empty q)) && not(Hashtbl.mem hash_table desired_state) && is_ok1 && is_ok2)
        do 
            let (moves, state) = Queue.pop q in 
                for i = 0 to n - 1
                do 
                    let state1 = empty state i in
                        new_state state1 (moves + 1);
                    let state2 = fill state i in
                        new_state state2 (moves + 1);
                    for j = 0 to n - 1
                    do
                        if i <> j then
                            let state3 = pour state i j in
                                new_state state3 (moves + 1);
                    done
                done
        done;
    );
    if not(Hashtbl.mem hash_table desired_state) then -1
    else Hashtbl.find hash_table desired_state;;
