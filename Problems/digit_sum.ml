(* Given inputs m and k, m, k > 1, list all numbers in base m whose digit sum = k. *)

(* 
    Essentially we want all (multi)sets that sum to k such that every element in the set is from {1, ... m-1}

    Specifications:
    Input: m, k in Z; m, k > 1
    Output: L, a list such that it only contains all multisets S (0 not in S) such that sum(S) = k. 
*)

let rec get_list (m : int) (k : int) (d : int) : int list = 
    match (m, k) with
    (m', _) when (m' <= 0) -> []
    | (_, k') when (k' = 0)  -> [0]          (* valid: digit sum matched, return sentinel *)
    | (_, k') when (k' < 0)  -> []           (* overshot: dead end *)
    | (m', k') ->
        List.concat_map                                                         (* Has the same effect as map then concatenate *)
            (fun d -> List.map (fun x -> x*10+d) (get_list m (k-d) d))          (* Takes each valid digit from the set S *)
            (List.init (m'-1) (fun i -> i+1))                                   (* Creates the set S {1, ..., m-1} *)
;;

let rec print_list (l : int list) : unit = 
    match l with
    [] -> ()
    | head::tail -> 
        print_int head;
        print_newline();
        print_list tail
;;

print_list(get_list 10 10 1)
