(* Lists *)

(* Lists are tuples of variable length *)
(* x1 :: (x2 ::(x3 :: [] ))) *)

type poly = float list

let q = [5.; 0.; 1.; 10.];;

let mulByX p = match p with
| [] -> []
| _ :: _ -> 0.0 :: p

let rec length xs = match xs with
| [] -> 0
| y ::ys -> 1 + length ys

let degree p = match p with
| [] -> 0
| x :: xs -> length (x :: xs) - 1

let rec pow x n = if n = 0
then 1.
else x *. (pow x (n - 1))

let rec evalAtAux ps x n = 
        match ps with
        | [] -> 0.
        | p :: ps -> p *. (pow x n) +. evalAtAux ps x (n + 1)

let evalAt ps x = evalAtAux ps x

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


