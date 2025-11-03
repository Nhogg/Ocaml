#use "w4.ml"

(* Trees *)
type 'a tree = Lf (* Leaf *)
        | Br of 'a * 'a tree * 'a tree (* Branch *)

(* SIze *)
let rec size t =
        match t with
        | Lf -> 0
        | Br (v, t1, t2) -> 1 + size t1 + size t2

let rec depth t =
        match t with
        | Lf -> 0
        | Br (v, t1, t2) -> 1 + max (depth t1) (depth t2)

let rec dft t =
        match t with
        | Lf -> []
        | Br (v, t1, t2) -> v :: append (dft t1) (dft t2)

let rec flatten t =
        match t with
        | Lf -> []
        | Br (v, t1, t2) -> append (flatten t1) (v :: flatten t2)


