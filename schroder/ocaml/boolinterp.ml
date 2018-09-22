let rec repl a = 
  let s = read_line () in
  print_string (s^"\n");
  repl ();;

repl ()
