(* Week 1 exercises *)

(* Square Root *)
let hyp a b =
        sqrt(a *. a +. b *. b)

(* GCD *)
let divides k n=
        n mod k = 0

let rec gcd m n =
        if n = 0
        then m
        else gcd n (m mod n)

let checkGT m n c =
        if divides c m && divides c n
        then c <= gcd m n
        else true
        
(* Even / Odd *)

let isEven n =
        if n mod 2 = 0 then true
        else false

let isOdd n =
        if isEven n = true then false
        else true

(* and not xor or *)
let myOr p q = if p then true else q

let myAnd p q = if p then q else false

let myNot p q = if p then false else true

let myXor p q = if p then (if q then false else true) else (if q then true else false)

(* pow *)

let rec pow m n =
        if n = 1 then m
        else
        m * pow m (n - 1)

(* collatz *)

let rec collatz n =
        if n = 1 then n else
        if n mod 2 = 0 then collatz (n / 2)
        else collatz (3 * n + 1)
(* LCM *)
let lcm m n =
        if n = 0
        then m else
        (m * n) / gcd m n















