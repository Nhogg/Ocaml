(* Algebraic Datatypes and Enums *)

type direction = N | E | S | W

let opposite d =
        match d with
        | N -> S
        | S -> N
        | E -> W
        | W -> E

let move coords dir =
        let (x, y) = coords in
        match dir with
        | N -> (x, y + 1)
        | E -> (x + 1, y)
        | S -> (x, y - 1)
        | W -> (x - 1, y)

type week = Mon | Tue | Wed | Thur | Fri | Sat | Sun

(* Paramaterized Constructors *)

type shape = 
        | Circle of (float * float) * float
        | Rectangle of (float * float) * (float * float)

let area s = 
        match s with
        | Circle ((x, y), r) -> 3.14 *. r *. r
        | Rectangle ((x1, y1), (x2, y2)) -> (x2 -. x1) *. (y2 -. y1)

let isSquare s =
        match s with
        | Circle ((x, y), r) -> false
        | Rectangle ((x1, y1), (x2, y2)) -> x2 -. x1 = y2 -. y1

(* Option Types *)
type 'a option = None | Some of 'a

let divide m n =
        if n = 0
        then None
        else
                Some (m / n)

let safeAdd m n =
        match m, n with
        | Some m, Some n -> Some (m + n)
        | None, Some n -> None
        | Some m, None -> None
        | None, Some n -> None

(* Unions and the Sum Type *)
type ('a, 'b) either = Left of 'a | Right of 'b

let strangeAdd m n =
        if m mod 2 = 0 && n mod 2 = 0
        then Right "Cannot add two even numbers"
        else if m mod 2 = 1 && n mod 2 = 1
        then Right "cannot add two odd numbers"
        else Left (m + 2)

(* Exercises *)

(* 1 *)
type vehicle = Bike | Motorbike | Car | Lorry

let wheels vehicleType =
        match vehicleType with
        | Bike -> 2
        | Motorbike -> 2
        | Car -> 4
        | Lorry -> 6

(* 2 *)
type newDir = N | E | S | W | NE | SE | NW | SW

let newOpp dir = 
        match dir with
        | N -> W
        | E -> S
        | S -> E
        | W -> N
        | NE -> SW
        | SE -> NW
        | NW -> SE
        | SW -> NE

(* 3 *)
let perimeter shp =
        match shp with
        | Circle ((x, y), r) -> 2. *. 3.14 *. r
        | Rectangle ((x1, y1), (x2, y2)) -> 2. +. (x2 -. x1) *. (y2 -. y1)

(* 4 *)
type tA = X of bool | Y of bool * bool
(**
Enumeration:
        X true
        X false
        Y true true
        Y false false
        Y true false
        Y false true
**)

type tB = X of bool * bool * bool | Y of bool * bool | Z of tA
(**
Enumeration:
        X has 8 possibilities
        Y has 4 possibilities
        tA has 6 possibilities
18 total elements
**)

(* 5 *)
type tA = XC of int | YC of bool
type tB = XD of bool | YD of int

let f_AB a =
        match a with
        | XC i -> YD i
        | YC b -> XD b

let g_BA b =
        match b with
        | XD b -> YC b
        | YD i -> XC i

type tC = bool * string * int
type tD = int * bool * string

let f_CD c =
        match c with
        | (b, s, i) -> (i, b, s)

let g_DC d =
        match d with
        | (i, b, s) -> (b, s, i)

type tE = XE of int * int | YE of int * int * int

type tF = int * int * (int option)

let f_EF e =
        match e with
        | XE (i1, i2) -> (i1, i2, None)
        | YE (i1, i2, i3) -> (i1, i2, Some i3)

let f_FE f =
        match f with
        |(i1, i2, None) -> XE (i1, i2)
        | (i1, i2, Some i3) -> YE (i1, i2, i3)

type tG = (int, bool) either * float

type tH = (int * float, bool * float) either

let f_GH g =
        match g with
        | (Left i, fl) -> Left (i, fl)
        | (Right b, fl) -> Right (b, fl)

let f_TH h =
        match h with
        | Left (i, fl) -> (Left i, fl)
        | Right (b, fl) -> (Right b, fl)



