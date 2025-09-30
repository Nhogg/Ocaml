let f x y = x + y * y

let g = fun x y -> x + y * y

let g x y z = x + 2 * y + 3 * z

let rec add1 xs = match xs with
        | [] -> []
        | x :: xs -> (x + 1) :: add1 xs

let rec add2 xs = match xs with
        | [] -> []
        | x :: xs -> (x + 2) :: add2 xs

let rec addN n xs = match xs with
        | [] -> []
        | x :: xs -> (((+) n) x) :: addN n xs

let rec mulN n xs = match xs with
        | [] -> []
        | x ::xs -> ((( * ) n) x) :: mulN n xs

let length xs = 
        let rec auxlen xs accum =
                match xs with
                | [] -> []
                | x :: xs -> auxlen xs (accum + 1)
        in auxlen xs 0

let rec applylen xs = match xs with
        | [] -> []
        | x :: xs -> (length x) :: applylen xs

let rec map f xs = match xs with
        | [] -> []
        | x :: xs -> (f x) :: map f xs

let addN n xs = map ((+) n) xs
let mulN n xs = map (( * ) n) xs





