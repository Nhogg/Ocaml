let twice n = n + n

let rec fact n = if n = 0
then 1
else n * fact (n - 1)

let rec gcd m n = if n = 0
then m
else gcd n (m mod n)

let add m n = m + n

let addIsComm m n = add m n = add n m

let divides k n = n mod k = 0

let addZero m = add m 0 = m

let gcdDiv m n = divides (gcd m n) m
&& divides (gcd m n ) n

let gcdGreatestm m  n c = if divides c m && divides c n
then c <= gcd m n
else true
