let rec small x ys =
        match ys with
        | [] -> []
        | y :: ys -> if y < x
                     then y :: small x ys
                     else small x ys
        
let rec large x ys =
        match ys with
        | [] -> []
        | y :: ys -> if x <= y
                     then y :: large x ys
                     else large x ys

let rec append xs ys =
        match xs with
        | [] -> ys
        | x :: xs -> x :: (append xs ys)

let rec qsort xs =
        match xs with
        | [] -> []
        | x :: xs -> let s = qsort (small x xs) in
                     let l = qsort (large x xs) in
                     append s (x :: l)

let rec filter p xs =
        match xs with
        | [] -> []
        | x :: xs -> if p x
                     then x :: filter p xs
                     else filter p xs

let rec map f xs =
        match xs with
        | [] -> []
        | x :: xs -> f x :: map f xs

let rec foldRight v f xs =
        match xs with
        | [] -> v
        | x :: xs -> f x (foldRight v f xs)

let rec foldLeft v f xs =
        match xs with
        | [] -> v
        | x :: xs -> foldLeft (f v x) f xs

(* Exercises *)

let rec squaresOfOdds numList =
        match numList with
        | [] -> []
        | x :: xs -> if (x mod 2 != 0) then x * x :: squaresOfOdds xs
                     else squaresOfOdds xs

let rec takeWhile func list =
        match list with
        | [] -> []
        | x :: xs ->
                        if func x then
                                x :: (takeWhile func xs)
                        else
                                []

let rec multiMap funList args =
        match funList, args with
        | [], [] -> []
        | x :: xs, y :: ys ->
                        x y :: multiMap xs ys

let rec applyN numTimes func value =
        if numTimes > 0 then
                (func value) + applyN (numTimes - 1) func value
        else 0

                        



        




