open Basesat;;


let rec checksat x =
    match x with
    | J(Cnt(s)) -> false
    | J(CONJ(s)) -> true
    | DF(s,more) ->
    begin
    match s with
    | Cnt(n) -> checksat more
    | CONJ(n) -> true
    end

let dosat_all x =
    let d = cnf2dnf x in
    let s = print_string(print_dnf d) in
    checksat d;;

let fn = (read_line ());;
let b = dosat_all (getCNFFromFile fn);;
if b then print_string "\nsat\n" else print_string "\nunsat\n";;
