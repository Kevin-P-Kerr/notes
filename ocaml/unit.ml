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

let doSat c = 
    doSatRec c [];;
