type atom = A of string;;
type negate = NEGATE;;
type atomProp = AP of atom | NAG of (negate*atom)
type disj = D of atomProp | DJ of (atomProp*disj)
type cnf = C of disj | CF (disj*cnf)
