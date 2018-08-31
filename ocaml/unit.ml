open Basesat;;

type unitcnf = CN of cnf | EMPTY;;
type unitreturn = L of atomProp list | FAIL;;

let elim cnf l =
    let a = getNextUnit cnf l in
    elimAtom cnf a;;

let elimAtom cnf a = 
    let b = propagateUnit cnf a in
    let c = checkResult b l in
    [a;b;c];;

let rec doSatRec c l =
    match c with 
    | EMPTY -> L(l)
    | CN(cnf) ->
        let e = elim cnf l in
        let b = List.nth e 2 in
        if not b then 
            let y = elimAtom cnf (negate a) in
            let bo = List.nth y 2 in
            if not bo then FAIL else doSatRec (List.nth y 2) (List.nth y 0)::l
        else
            doSatRec (List.nth e 2) (List.nth e 0)::l;;

let doSat c = 
    doSatRec c [];;
