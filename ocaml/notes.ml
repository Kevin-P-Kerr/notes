type constant = string;;
type variable = string;;
type formal_number = C of constant | V of variable;;
type op = PLUS | MINUS | TIMES | DIVIDE;;
type eq = EQUALS;;
type arithexpr = F of formal_number | A of formal_number * op * arithexpr;;
type formula = FO of arithexpr * eq * arithexpr;;

let z v = FO (A((V v), PLUS,  F(C("0"))),EQUALS ,F(V(v)));;
let r va vb = FO (A(V(va), PLUS, F(V(vb))), EQUALS, A(V(vb), PLUS, F(V(va))));;
