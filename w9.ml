#use "w4.ml"
(* Examples *)

type aexp = Num of int
          | Sum of aexp * aexp
          | Prod of aexp * aexp
          | Neg of aexp

(* Thi implmentation does not support paranthesis! *)
let rec printAExp ae =
        match ae with
        | Num n -> Int.to_string n
        | Sum (e1, e2) -> printAExp e1 ^ " + " ^ printAExp e2
        | Prod (e1, e2) -> printAExp e1 ^ " * " ^ printAExp e2
        | Neg e -> " - " ^ printAExp e

let parens s = "(" ^ s ^ ")"

let rec printAExp ae =
         match ae with
         | Num n -> Int.to_string n
         | Sum  (e1, e2) -> parens (printAExp e1 ^ " + " ^ printAExp e2)
         | Prod (e1, e2) -> parens (printAExp e1 ^ " * " ^ printAExp e2)
         | Neg e -> parens ("- " ^ printAExp e)

let rec eval ae =
        match ae with
        | Num n -> n
        | Sum (e1, e2) -> eval e1 + eval e2
        | Prod (e1, e2) -> eval e1 * eval e2
        | Neg e -> - (eval e)

(* Documents *)

type content = Empty | Elt of elt * content
and elt = Par of string | Sec of string * content

type doc = string * string * string

(* Example Documents *)
let c1 =
 Elt ( Sec ( "Section 1"
           , Empty
           )
     , Elt ( Sec ( "Section 2"
                 , Empty
                 )
           , Elt ( Sec ( "Section 3"
                       , Empty
                       )
                 , Empty
           )
     )
 )

 let c2 =
  Elt ( Sec ( "Section 1"
            , Elt ( Par "sample text"
                  , Empty
                  )
            )
      , Elt ( Sec ( "Section 2"
                  , Empty
                  )
            , Elt ( Sec ( "Section 3"
                        , Empty
                        )
                  , Empty
                  )
            )
      )


  let c3 =
  Elt ( Sec ( "Section 1"
            , Elt ( Par "sample text"
                  , Empty
                  )
            )
      , Elt ( Sec ( "Section 2"
                  , Elt ( Sec ( "Section 2.1", Empty)
                        , Elt ( Sec ( "Section 2.2", Empty)
                              , Empty
                              )
                        )
                  )
            , Elt ( Sec ( "Section 3"
                        , Empty
                        )
                  , Empty
                  )
            )
      )


(* Table of contents generation *)
  let increase s = "#" ^ s

  let rec tocC prefix ct =
          match ct with
          | Empty -> []
          | Elt (e, ct) -> append (tocE prefix e) (tocC prefix ct)
  and tocE prefix e =
          match e with
          | Par _ -> []
          | Sec (t, ct) -> (prefix ^ " " ^ t) :: tocC (increase prefix) ct

(* Since content is just a list of elements, we can redefine the constructor *)
type elt = Par of string | Sec of string * elt list
type doc = string * string * elt list

let rec concatMap f xs =
        match xs with
        | [] -> []
        | x :: xs -> append (f x) (concatMap f xs)

let rec tocE prefix e =
        match e with
        | Par _ -> []
        | Sec (t, es) -> (prefix ^ t) :: concatMap (tocE (increase prefix)) es

let toc (_, _, es) = concatMap (tocE "# ") es

(* Rose Trees *)
type 'a rosetree = Node of 'a * 'a rosetree list

type 'a rtree = NodeT of 'a * 'a forest
and 'a forest = Empty | Trees of 'a rtree * 'a forest

(* Mapping over trees *)

(* Binary trees *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let rec mapT f t =
        match t with
        | Lf -> Lf
        | Br (x, lt, rt) -> Br (f x, mapT f lt, mapT f rt)

(* Rose trees *)
let rec mapRT f t = 
        match t with
        | Node (x, ts) -> Node (x, map (mapRT f) ts)
and mapFT f ts =
        match ts with
        | [] -> []
        | t :: ts -> mapRT f t :: mapFT f ts





