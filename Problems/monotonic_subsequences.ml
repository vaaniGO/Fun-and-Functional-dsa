(* Given n = k^2, construct a sequence that has no monotonic subsequence of length > k + 1. *)

(*
  Specifications:
  Input: n, k; k < n; n, k > 1; n, k in Z 
  Output: list L such that for all sorted sets S = {a_1, a_2, ... a_i}, where each a_j is an index in 
          L, i > k + 1 <=> L(a_1), ..., L(a_i) is NOT monotonic. 
*)

let rec getList (k : int) : int list =
  match k with
  | k' when k' <= 1 -> []
  | k' ->
    List.init k' (fun i -> i + 1)           (* [1, 2, ..., k] *)
    |> List.map (fun x -> x * k')           (* [k, 2k, ..., k^2] *)
    |> List.concat_map (fun start ->
         List.init k' (fun i -> start - i)) (* each start gives [start, start-1, ..., start-(k-1)] *)
;;

let rec print_list (l : int list) : unit = 
    match l with
    [] -> ()
    | head::tail -> 
        print_int head;
        print_newline();
        print_list tail
;;

print_list(getList 3);

(* 3 2 1 
   6 5 4 
   9 8 7 *)
