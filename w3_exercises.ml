(* Week 3 exercises (lists) *)

let mulByX vs =
        match vs with
        | [] -> []
        | v :: vs -> 0.0 :: v :: vs

let valueAt ps x =
        let rec aux xn ps =
                match ps with
                | [] -> 0.0
                | p :: ps -> p *. xn +. aux (xn *. x) ps
        in aux 1.0 ps

let degree ps =
        let rec aux ps =
                match ps with
                | [] -> 0
                | _ :: ps -> 1 + aux ps
        in match ps with
        | [] -> 0
        | _ :: ps -> aux ps

let rec length xs =
        match xs with
        | [] -> 0
        | _ :: xs -> 1 + length xs

let rec append xs ys =
        match xs with
        | [] -> ys
        | x :: xs -> x :: (append xs ys)

let rec zip xs ys =
        match xs, ys with
        | x :: xs, y :: ys -> (x, y) :: zip xs ys
        | [] , [] -> []

let rec reverse xs =
        match xs with
        | [] -> []
        | x :: xs -> append (reverse xs) [x]

let len xs = 
        let rec aux xs n =
                match xs with
                | [] -> n
                | _ :: xs -> aux xs (n + 1)
        in aux xs 0

let rec unzip xy = 
        match xy with
        | x :: y -> [x] :: [y]
        | [] -> []

let rec fromMtoN x y =
        if x > y then
                []
        else
                x :: fromMtoN ( x + 1 ) y

let upto n =
        fromMtoN 0 n

(* 
let rec take n xs = 
        match n, xs with
        | 0, _ -> []
        | _, [] -> []
        | _, x :: xs_tail -> x :: (take (n - 1) xs_tail)
*)

let rec take n xs = 
        if n <= 0 then []
        else
                match n, xs with
                | _, [] -> []
                | _, x :: xs_tail -> x :: (take (n - 1) xs_tail)

let rec drop n xs =
        if n <= 0 then xs
        else
                match n, xs with
                | _, [] -> []
                | _, x :: xs_tail -> drop (n - 1) xs_tail

let divisorOf n =
        let rec aux currentNum acc =
                if currentNum > n then
                        acc
                else
                        if n mod currentNum = 0 then
                                aux (currentNum + 1) (currentNum :: acc)
                        else
                                aux (currentNum + 1) acc
        in
        let result = aux 1 []
in result


