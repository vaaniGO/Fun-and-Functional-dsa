
(*
  Here is the basic idea of the code: If we are working with a crazy no. of rounds like 10^16 we work with log(ln(10^16)). 
  we perform binary search to find the smallest number remaining after 10^16 rounds - 
  this helps us calculate precisely the number of times each number was sqaured 
  in a chain of squarings - but we perform binary search on the stored double logged value instead of on the large threshold.
*)

(* We want to guess the threshold t and keep adjusting *)

let p = 1234567891


let rec powmod base exp md =
  if exp = 0 then 1
  else
    let h = powmod base (exp / 2) md in
    let h2 = h * h mod md in
    if exp land 1 = 1 then h2 * base mod md else h2
  ;;


let log2 z = log z /. log 2.

let arr = Array.init (limit - 1) (fun i -> i + 2)

let rec get_rounds (t : float) (index : int) (rounds : int) (sum : int) : (int * int) =
    if index = Array.length arr then (rounds, sum)
    else
      let current = arr.(index) in
      let x = max 0 (int_of_float (floor (t -. log2 (log (float_of_int current)) +. 1e-7)) + 1) in
      let value = powmod current (powmod 2 x (p - 1)) p in
      get_rounds t (index + 1) (rounds + x) ((sum + value) mod p)
    ;;

let rec compute (low : float) (high : float) : float =
    if high -. low < 1e-9 then high
    else
      let mid = (low +. high) /. 2. in
      let (r, _) = get_rounds mid 0 0 0 in
      if r >= m then compute low mid else compute mid high
    ;;

let solve (limit : int) (m : int) : int =

  let t = compute (-1.0) (float_of_int m /. float_of_int (limit - 1) +. 2.0) in
  let (rounds_hi, sum_hi) = get_rounds t 0 0 0 in
  let overshoot = rounds_hi - m in
  if overshoot = 0 then sum_hi
  else begin
    let best = ref neg_infinity in
    Array.iter (fun num ->
      let c = max 0 (int_of_float (floor (t -. log2 (log (float_of_int num)) +. 1e-7)) + 1) in
      if c >= 1 then
        (let q = float_of_int (c - 1) +. log2 (log (float_of_int num)) in
         if q > !best then best := q)) arr;
    let theta = ref (-1) in
    Array.iter (fun num ->
      let c = max 0 (int_of_float (floor (t -. log2 (log (float_of_int num)) +. 1e-7)) + 1) in
      if c >= 1 then
        (let q = float_of_int (c - 1) +. log2 (log (float_of_int num)) in
         if abs_float (q -. !best) < 1e-6 && !theta < 0 then
           theta := powmod num (powmod 2 (c - 1) (p - 1)) p)) arr;
    let th = !theta in
    let th2 = th * th mod p in
    (((sum_hi - overshoot * th2 + overshoot * th) mod p) + p) mod p
  end