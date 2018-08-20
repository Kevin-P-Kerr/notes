open Basesat;;

let distributetoconj d cj =
  match cj with
  | Cnt(c) -> cj
  | CONJ(c) -> 
      match c with
      | CJ(ap) ->
          if debug(toatomstr d^" "^(toatomstr ap)) (contradicts d ap) then Cnt(CONTRARY) else CONJ(MCJ(d,c))
      | MCJ(ap,more) -> 
          if (contradicts d ap) || (contradictsany d more) then Cnt(CONTRARY) else CONJ(MCJ(d,c))
;;

let rec distributeover d p =
  match p with 
  | J(cj) -> J(distributetoconj d cj)
  | DF(cj,df) -> 
      let nc = distributetoconj d cj in
      let rest = distributeover d df in
      DF(nc,rest);;

let rec echocnf2dnf d = 
  match d with 
  | C(D(d)) -> J(CONJ(CJ(d)))
  | C(DJ(a,b)) -> DF(CONJ(CJ(a)), echocnf2dnf (C(b)));;

let rec cnf2dnf c = 
  match c with 
  | C(d) -> echocnf2dnf c
  | CF(d,e) ->
      let p = cnf2dnf e in
      match d with 
      | D(ap) -> (distributeover ap p)
      | DJ(ap,dj) ->
          let rec helper dis =
            match dis with
            | D(at) -> distributeover at p
            | DJ(aap,ddj) -> 
                concatdnf (distributeover aap p) (helper ddj)
          in 
          let z = helper dj in
          concatdnf (distributeover ap p) z;;

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
    checksat (cnf2dnf x)

let fn = (read_line ());;
let b = dosat_all (getCNFFromFile fn);;
if b then print_string "sat\n" else print_string "unsat\n";;
