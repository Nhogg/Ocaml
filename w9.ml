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
