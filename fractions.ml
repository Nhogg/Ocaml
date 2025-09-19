(* fractions *)

(* reciprocal *)
let reciprocal frac = let (x, y) = frac in
(y, x)

(* isValid *)
let isValid frac = let (x, y) = frac in
if y = 0 then false
else true

(* fracToFloat *)
let fracToFloat frac = let (x, y) = frac in
if isValid(frac) then float_of_int(x) /. float_of_int(y)  else 0.


(* Simplify *)
(* gcd aux *)
let rec gcd m n = if n = 0
then m
else gcd n ( m mod n)

let simplify frac = let (x, y) = frac in
if isValid frac then (x / gcd x y, y / gcd x y)
else (0, 0)

(* add *)
let add (frac1, frac2) = let ((a, b), (c,d)) = (frac1, frac2) in
if isValid (a, b) && isValid (c, d) then
       simplify (a * d + c * b, b * d)
else (0, 0)

(* Multiply *)
let multiply (frac1, frac2) = let ((a, b), (c, d)) = (frac1, frac2) in
if isValid (a, b) && isValid (c, d) then
        simplify (a * c, b * d)
else (0, 0)
