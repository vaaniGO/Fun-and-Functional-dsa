(* Out of the first 617 thousand sqaures, sum the odd ones only *)

let rec getSum (current : int) (count : int) : int =
  match count with 
  | ct when ct >= 617000 -> 0
  | _ -> current*current + getSum (current + 2) (count + 2)
;;

print_int (getSum 1 0);
print_newline ()