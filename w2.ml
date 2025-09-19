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

let rec fac n = if n = 0
then 1
else n * fac (n - 1)

let sum n = 
let rec auxsum p = let (acc, n) = p in 
        if n = 0
        then acc
        else auxsum (acc + n, n - 1)
in auxsum (0, n)

let max3 p = let (x, y, z) = p in
(if x > y && x > z then x 
        else if y > x && y > z then y 
        else z + 0)

let time f x = 
        let t = Sys.time() in
        let fx = f x in
        Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
        fx


let rec fastPow n k = if k = 0
then 1
else if k mod 2 = 0
then fastPow (n * n) (k / 2)
else fastPow (n * n) (k / 2) * n

let rec fib n = if n = 0
then 0
else if n = 1
then 1
else fib (n - 1) + fib (n - 1)

let step p = let (a, b) = p in (b, a + b)

let rec minifib n = if n = 0
then  (0, 1)
else step(minifib (n - 1))


let rec fastfib n = let (x, _) = minifib n in x






