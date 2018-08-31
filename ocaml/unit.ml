open Basesat;;

type unitcnf = CN of cnf | EMPTY;;
type unitreturn = L of atomProp list | FAIL;;

let elim cnf l =
    let a = getNextUnit cnf l in
    elimAtom cnf a;;

let elimAtom cnf a = 
    let b = propagateUnit cnf a in
    [a;b];;

let negate a = 
    match a with
    | AP(at) -> NAG(NEGATE,at)
    | NAG(n,at) -> at;;

let checkAtomConsistency a1 a2 =
    match a1 with
    | AP(a) ->
        begin
        match a2 with
        AP(aa) -> true
        NAG(n,aa) -> if (atomlit aa) = (atomlit a) then false else true 
        end
    | NAG(n,a) ->
        begin
        match a2 with
        |NAG(n,aa) -> true
        | AP(aa) -> if (atomlit aa) = (atomlit a) then false else true
        end;;

let rec checkConsistency l =
    let helper l a = 
        match l with
        | x::xs -> if not checkAtomConsistency x a then false else helper xs a
        | [] -> true
        in
    match l with
    | [] -> true
    | x::xs -> if helper x xs then checkConsistency xs else false;;

let inconsistent cnf l =
    checkConsistency (List.concat[(collectUnits cnf); l]);;

let rec doSatRec c l =
    match c with 
    | EMPTY -> L(l)
    | CN(cnf) ->
        if inconsistent cnf l then FAIL else
            let y = elim cnf l in
            let r = doSatRec (List.nth y 1) (List.nth y 0)::l in
            match r with
            | FAIL ->
                begin
                let a = (List.nth y 0) in
                let na = negate a in
                let ny = elimAtom cnf a in
                doSatRec(List.nth ny 1) a::l
                end
            | L(ap) -> ap;;

let doSat c = 
    doSatRec c [];;
