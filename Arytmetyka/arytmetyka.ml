(*Autor: Patryk Malcher grupa 1
    w parze z Łukasz Łopacki grupa 4*)
type wartosc = {a : float ; b : float ; c : bool};;

let is_nan x = compare x nan = 0;;

let min a b = 
    if is_nan a then b 
    else if is_nan b then a
    else if a < b then a
    else b;;

let max a b =
    if is_nan a then b
    else if is_nan b then a
    else if a > b then a 
    else b;;

let min4 a b c d =
    min (min (min a b) c) d;;

let max4 a b c d =
    max (max (max a b) c) d;;

let wartosc_dokladnosc x p = 
    let help = x *. p /. 100. in 
        {a = min (x -. help) (x +. help) ; b = max (x -. help) (x +. help) ; c = false};;

let wartosc_od_do x y = 
    {a = x ; b = y ; c = false};;

let wartosc_dokladna x = 
    {a = x ; b = x ; c = false};;

let in_wartosc w x = 
    if is_nan w.a then false 
    else if w.c = false then
        if x >= w.a && x <= w.b then true
        else false 
    else 
        if x > w.a && x < w.b then false
        else true;;

let min_wartosc w = 
    if is_nan w.a then nan
    else if w.c = false then w.a 
    else if w.a <> neg_infinity then neg_infinity 
    else w.b 

let max_wartosc w =
    if is_nan w.a then nan
    else if w.c = false then w.b 
    else if w.b <> infinity then infinity 
    else w.a

let sr_wartosc w = 
    if w.c = false then (w.a +. w.b) /. 2.
    else nan;;

let plus x y = 
    if is_nan x.a || is_nan y.a then {a = nan ; b = nan ; c = false}
    else if x.c = true && y.c = true then {a = neg_infinity ; b = infinity ; c = false}
    else if x.c = false && y.c = false then {a = x.a +. y.a ; b = x.b +. y.b ; c = false}
    else 
        if x.c = true then 
            if x.a +. y.b >= x.b +. y.a then {a = neg_infinity ; b = infinity ; c = false}
            else {a = x.a +. y.b ; b = x.b +. y.a ; c = true}
        else 
            if y.a +. x.b >= y.b +. x.a then {a = neg_infinity ; b = infinity ; c = false}
            else {a = y.a +. x.b ; b = y.b +. x.a ; c = true};;

let minus x y = 
    if is_nan x.a || is_nan y.a then {a = nan ; b = nan ; c = false}
    else if x.c = true && y.c = true then {a = neg_infinity ; b = infinity ; c = false}
    else if x.c = false && y.c = false then {a = x.a -. y.b ; b = x.b -. y.a ; c = false}
    else 
        if x.c = true then 
            if x.a -. y.a >= x.b -. y.b then {a = neg_infinity ; b = infinity ; c = false}
            else {a = x.a -. y.a ; b = x.b -. y.b ; c = true}
        else 
            if x.b -. y.b >= x.a -. y.a then {a = neg_infinity ; b = infinity ; c = false}
            else {a = x.b -. y.b ; b = x.a -. y.a ; c = true};;

let razy_help x y = 
    if y.a *. y.b < 0. && y.c = false then {a = neg_infinity ; b = infinity ; c = false}
    else if y.a < 0. && y.b < 0. then let mini = min (x.a *. y.a) (x.a *. y.b) in let maxi = max (x.b *. y.a) (x.b *. y.b) in 
        if maxi >= mini then {a = neg_infinity ; b = infinity ; c = false} 
        else {a = maxi ; b = mini ; c = true}
    else if y.a < 0. && y.b > 0. then let mini = min (x.a *. y.a) (x.b *. y.b) in let maxi = max (x.b *. y.a) (x.a *. y.b) in 
        if maxi >= mini then {a = neg_infinity ; b = infinity ; c = false} 
        else {a = maxi ; b = mini ; c = true} 
    else if y.a > 0. && y.b < 0. then let mini = min (x.b *. y.a) (x.a *. y.b) in let maxi = max (x.a *. y.a) (x.b *. y.b) in 
        if maxi >= mini then {a = neg_infinity ; b = infinity ; c = false} 
        else {a = maxi ; b = mini ; c = true}
    else let mini = min (x.b *. y.a) (x.b *. y.b) in let maxi = max (x.a *. y.a) (x.a *. y.b) in 
        if maxi >= mini then {a = neg_infinity ; b = infinity ; c = false} 
        else {a = maxi ; b = mini ; c = true};;

let razy x1 y1 =
    let x = if x1.c = true && x1.a = neg_infinity then {a = x1.b ; b = infinity ; c = false} 
                else if x1.c = true && x1.b = infinity then {a = neg_infinity ; b = x1.a ; c = false}
                    else x1 in 
    let y = if y1.c = true && y1.a = neg_infinity then {a = y1.b ; b = infinity ; c = false} 
                else if y1.c = true && y1.b = infinity then {a = neg_infinity ; b = y1.a ; c = false}
                    else y1 in
    if is_nan y.a || is_nan x.a then {a = nan ; b = nan ; c = false}
    else if (y.a = 0. && y.b = 0.) || (x.a = 0. && x.b = 0.) then {a = 0. ; b = 0. ; c = false}
    else if x.c = false && y.c = false then {a = min4 (x.a *. y.a) (x.a *. y.b) (x.b *. y.a) (x.b *. y.b) ; b = max4 (x.a *. y.a) (x.a *. y.b) (x.b *. y.a) (x.b *. y.b); c = false}
    else if x.c = true && y.c = true && (x.a > 0. || y.a > 0. || x.b < 0. || y.b < 0.) then {a = neg_infinity ; b = infinity ; c = false}
    else 
        if x.c = true then
            razy_help x y
        else  
            razy_help y x;;

let podzielic x y = 
    let help = y.c in
    let help2 = (help <> true) in 
    if (y.a = 0. && y.b = 0.) || is_nan y.a || is_nan x.a then {a = nan ; b = nan ; c = false}
    else if (x.a = 0. && x.b = 0.) then {a = 0. ; b = 0. ; c = false}
    else if y.a = neg_infinity && y.b = infinity then {a = neg_infinity ; b = infinity ; c = false}
    else if y.a *. y.b > 0. then razy x {a = 1. /. y.b ; b = 1. /. y.a ; c = help}
    else if y.b = 0. then razy x {a = neg_infinity ; b = 1. /. y.a ; c = help}
    else if y.a = 0. then razy x {a = 1. /. y.b ; b = infinity ; c = help}
    else razy x {a = 1. /. y.a ; b = 1. /. y.b ; c = help2};;

