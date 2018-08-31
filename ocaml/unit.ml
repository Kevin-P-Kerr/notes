open Basesat;;

type empcon = EMPTY | CONFLICT;;
type unitcnf = CN of cnf | E of empcon;;
type unitdj = DD of disj | EC of empcon;;
type unitreturn = L of atomProp list | FAIL;;

let getNextUnit cnf =
    match cnf with
    |C(dj) ->
        begin
        match dj with
        | D(a) -> a
        | DJ(a,dj) -> a
        end
    |CF(dj,cf) ->
        begin
        match dj with
        |D (a) -> a
        |DJ (a,dj) -> a
        end;;

let rec propagateIntoDJ a dj= 
    match dj with
    | D(ap) -> if (contradicts ap a) then EC(CONFLICT) else if (litequals ap a) then EC(EMPTY) else DD(dj)
    | DJ(ap,dj) ->  
        let b = (contradicts ap a) in
        if ((not b) && (litequals ap a)) then EC(EMPTY) else let r = propagateIntoDJ a dj in
    match r with 
    | EC(ec) -> ec
    | DD(ddj) -> if b then r else DD(DJ(ap,ddj));;

let rec propagateUnit cnf a =
    match cnf with
    E(e) -> e
    | CN(cf) ->
        begin
        match cf with
        |C(dj) -> let d = propagateIntoDJ a dj in
        begin
        match d with
        | EC(EMPTY) -> E(EMPTY)
        | EC(CONFLICT) -> E(CONFLICT)
        | DD(ddj) -> CN(C(ddj))
        end
        end
        |CF(dj,cf) -> let d = propagateIntoDJ a dj in
        begin
        match d with
        | EC(CONFLICT) -> E(CONFLICT)
        | EC(EMPTY) -> propagateUnit cf a
        | DD(ddj) -> 
            begin
            let rem = propagateUnit cf a in
            match rem with
            | E(CONFLICT) -> rem 
            | E(EMPTY) -> CN(C(ddj))
            | CN(cff) -> CN(CF(ddj,ccf))
            end
        end;;

let elim cnf  =
    let a = getNextUnit cnf in
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
        |AP(aa) -> true
        |NAG(n,aa) -> if (atomlit aa) = (atomlit a) then false else true 
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
    | CONFLICT -> FAIL
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
