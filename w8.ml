#use "w4.ml"

(* Trees *)
type 'a tree = Lf (* Leaf *)
        | Br of 'a * 'a tree * 'a tree (* Branch *)

(* Size *)
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

let rec memberBst x t =
        match t with
        | Lf -> false
        | Br (v, t1, t2) -> if x = v 
                            then true
                            else if x < v
                            then memberBst x t1
                            else memberBst x t2

let rec add x t =
        match t with
        | Lf -> Br (x, Lf, Lf)
        | Br (v, t1, t2) -> if x <= v
                            then Br (v, add x t1, t2)
                            else Br (v, t1, add x t2)

        | [] -> Lf
        | y :: ys -> add y (toTree ys)

let treeSort xs = flatten (toTree xs)

(* Exercises *)

(* 1 *)
let rec sumT tree = 
        match tree with
        | Lf -> 0
        | Br (v, t1, t2) -> v + sumT t1 + sumT t2

(* 2 *)
let rec mirror tree = 
        match tree with
        | Lf -> Lf
        | Br (v, t1, t2) -> Br (v, mirror t1, mirror t2)

let rec member value tree =
        match tree with
        | Lf -> false
        | Br (v, t1, t2) -> if v = value
                            then true
                            else if v < value then
                                    member value t1
                            else
                                    member value t2

let rec mapT func tree =
        match tree with
        | Lf -> Lf
        | Br (v, t1, t2) -> Br (func v, mapT func t1, mapT func t2)

let rec dftFast tree =
        let rec dftAux tree accum =
               match tree with
               | Lf -> accum
               | Br (v, t1, t2) -> v :: (dftAux t1 (dftAux t2 accum)) 
        in
        dftAux tree []

let rec isBst tree =
        let rec aux tree min max =
                match tree with
                | Lf -> true
                | Br (v, t1, t2) ->
                                let withinBounds =
                                        (match min with
                                        | None -> true
                                        | Some minv -> v > minv)
                                        &&
                                        (match max with
                                        | None -> true
                                        | Some maxv -> v < maxv)
                                in
                                withinBounds
                                && aux t1 min (Some v)
                                && aux t2 (Some v) max
        in
        aux tree None None

let rec isAlmostList tree =
        match tree with
        | Lf -> true
        | Br (_, Lf, Lf) -> true
        | Br (_, Lf, r) -> isAlmostList r
        | Br (_, l, Lf) -> isAlmostList l 
        | Br (_, l, r) -> false


