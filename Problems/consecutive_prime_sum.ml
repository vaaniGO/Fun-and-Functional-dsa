
(* Array-based sieve: mutates in place — no list allocation overhead *)
let sieve =
  let s = Array.make 1000001 true in
  s.(0) <- false; s.(1) <- false;
  let i = ref 2 in
  while !i * !i <= 1000000 do
    if s.(!i) then begin
      let j = ref (!i * !i) in
      while !j <= 1000000 do
        s.(!j) <- false;
        j := !j + !i
      done
    end;
    i := !i + 1
  done;
  s
;;

(* Build prime list from the sieve *)
let pList =
  let acc = ref [] in
  for i = 1000000 downto 2 do
    if sieve.(i) then acc := i :: !acc
  done;
  !acc
;;

(* O(1) prime check directly off the sieve array *)
let isPrimeArr (n : int) : bool =
  n >= 2 && n <= 1000000 && sieve.(n)
;;

(* Prefix sums array: prefixArr.(i+1) - prefixArr.(i) = pList[i]
   Sum of window [i, i+w) = prefixArr.(i+w) - prefixArr.(i) *)
let prefixArr =
  let pArr = Array.of_list pList in
  let n = Array.length pArr in
  let prefix = Array.make (n + 1) 0 in
  for i = 0 to n - 1 do
    prefix.(i + 1) <- prefix.(i) + pArr.(i)
  done;
  prefix
;;

(* Find the prime below 1,000,000 that is a sum of the most consecutive primes.
   Iterates window sizes from largest down, breaking early when sums exceed 1M. *)
let findAnswer () =
  let n = Array.length prefixArr - 1 in
  let best_w = ref 0 in
  let best_prime = ref 0 in
  for w = n downto 1 do
    if w > !best_w then
      let i = ref 0 in
      while !i <= n - w do
        let s = prefixArr.(!i + w) - prefixArr.(!i) in
        if s >= 1000000 then i := n  (* prune: sum already too large *)
        else begin
          if isPrimeArr s && w > !best_w then begin
            best_w := w;
            best_prime := s
          end;
          i := !i + 1
        end
      done
  done;
  !best_prime
;;

let () =
  print_int (findAnswer ());
  print_newline ()
;;