(*
  The finite version of Schur’s Theorem says that, for all r there exists a number N = f(r)
  such that if each element of {1, 2, ... N} is coloured using one of the r colours, 
  then there exists a monochromatic solution to x + y = z.

  (1) Given r, N, C when C is the colouring, determine if it is invalid (i.e. no solution exists). 

  (2) Given r, N output an invalid colouring.
*)

(*
(1)
  Specifications:
  Input: r, N, C; r, N > 1; r, N in Z; C is a List L such that |L| = N and L[i] = color(i); color(i) in {1, ... r} forall i
  Output: true iff C has a monochromatic solution to x + y = z.

  The key idea is that we can create at most r other lists - each corresponding to one color only 
  Within these lists, we sort the indices using some fast sorting method 
  and then we can use a two pointer within each list to check for a solution to x + y = z.
*)

let rec helper_checkTwoSum (l : int list) (rl : int list) (target : int) : bool =
  match (l, rl) with
  | ([], _) | (_, []) -> false
  | (x :: _, y :: _) when x >= y -> false        (* pointers have crossed *)
  | (x :: _, y :: _) when x + y = target -> true
  | (x :: rest, y :: _) when x + y < target -> helper_checkTwoSum rest rl target
  | (_ :: _, _ :: rest) -> helper_checkTwoSum l rest target  (* sum too big, shrink right *)
;;

let rec helper_checkMonochromeSolution (l : int list) (right : int) : bool = 
  match right with 
  | 0 -> false 
  | _ -> 
    if helper_checkTwoSum l (List.rev l) (List.nth l right)   (* right-1 so we don't include L[right] itself *)
    then true 
    else helper_checkMonochromeSolution l (right - 1)
;;

let hasSolution (r : int) (n : int) (c : int list) : bool = 
  match (n, r) with
  | (_, r') when r' < 1 -> false
  | (n', _) when n' < 1 -> false
  | _ ->
      let indexed = List.mapi (fun idx color -> (idx, color)) c in
      let lists = List.init r (fun i ->
        indexed
        |> List.filter (fun (_, color) -> color = i)     (* keep only this color *)
        |> List.sort (fun (idx1, _) (idx2, _) -> compare idx1 idx2)  (* sort by index *)
        |> List.map (fun (idx, _) -> idx + 1)            (* extract 1-based index as the value *)
      ) in
      let results = List.map (fun l -> helper_checkMonochromeSolution l (List.length l - 1)) lists in
      List.fold_left (fun acc x -> acc || x) false results
;;


