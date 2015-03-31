(* Learning Ocaml *)

(*  *)

let rec range a b =
  if a > b then []
  else a :: range (a+1) b

let _ =
  print_int (List.fold_left (+) 0 (List.filter (fun x -> (x mod 3) = 0 || (x mod 5) = 0) (range 1 999)));
  print_newline ()
