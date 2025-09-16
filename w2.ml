(* Tuples *)

let max2 p = let (x, y) = p in
(if x > y then x + 0 else y)


let around n = (n - 1, n + 1);;

let swap p = let (x, y) = p in
(y, x)

(* Tuples for faster computation *)
let rec sum n = if n = 0
        then 0
else n + sum ( n - 1)

let tsum n = let auxsum accum k = if k = 0
then accum
else auxsum (accum + k) (k - 1)
in auxsum 0 n
