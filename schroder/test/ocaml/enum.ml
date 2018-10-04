let two2eight = 2*2*2*2*2*2*2*2;;

let getPow i =
  let rec helper x y =
    if x=0 then y else helper (x-1) (y*2)
  in
  helper i 1;;


(* a=11
 * b=10
 * c=01
 * d=00
 *)
let getTableArray i =
  let rec helper a n =
    if n=8 then a else
      let p = getPow n in
      let v = i land p in
      let aa = v::a in 
      helper aa (n+1)
  in
  helper [] 0;;

let printttentry a b =
  let sa = if a=0 then "0" else "1" in
  let sb = if b=0 then "0" else "1" in
  let s = sa^sb in
  print_string(s);
  s;;

let printbin a =
  let rec helper a s =
    match a with
    | [] -> s
    | x::xs -> 
        let v = if x>0 then "1" else "0" in
        helper xs (s^v)
    in
    let s = helper a "" in
    print_string(s);;

let printtt i =
  let t = getTableArray i in
  let aa = List.nth t 0 in
  let ab = List.nth t 1 in
  let ba = List.nth t 2 in
  let bb = List.nth t 3 in
  let ca = List.nth t 4 in
  let cb = List.nth t 5 in
  let da = List.nth t 6 in
  let db = List.nth t 7 in 
  print_string("----------------\n");
  printbin t;
  print_string "\n";
  print_string("00 | ");
  printttentry da db;
  print_string("\n01 | ");
  printttentry ca cb;
  print_string("\n10 | ");
  printttentry ba bb;
  print_string("\n11 | ");
  printttentry aa ab;
  print_string("\n");;

let printall u =
  let rec helper i =
    if i = two2eight then () else begin printtt i; helper (i+1); end in
  helper 0;;

printall ();;
