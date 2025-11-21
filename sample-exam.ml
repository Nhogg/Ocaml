(* Question 1 *)

(* A *)
(**
   Provide the definition of the `a option type, explaining
   the constructors that it contains. Briefly explain a use-case
   for htis type by defining a function divide: int -> int -> int option
   which divides the first argument by the second, such that it does not
   throw a division-by-zero exception for bad inputs.
**)

let divide a b =
  if b == 0.0 then
    0.0
  else
    a /. b


(* B *)
(**
  Suppose you have been given a version of OCaml that
  does not come bundled wiht the 'a list type. Define a new
type, 'a mylist, as an algebraic datatype. It should contain
two constructors called Nil and Cons - expain how these two
constructors mimic the usual list type in OCaml. Which values
in the type int mylist do the lists [1;2;3] and [] correspond to?
**)


(* C *)
(**
  For the type 'a mylist in Part (B), define:
    length : 'a mylist -> int
  and
    sum : int mylist -> int
  which calculate the length and sum-of-values of their respective
  inputs
**)


(* Question 2 *)

(* A *)
(**
  Define the function append : 'a list -> 'a list -> 'a list
  for lists, explaining the idea behind the recursive definition
**)


(* B *)
(**
  Provide the execution trace for append [1;2;3] [4;5;6]
  Supposing xs and ys are lists of lengths m and n respectively,
  how many recursive calls does the computation append xs ys make?
  Expalin your reasoning.
**)


(* C *)
(**
  There are two ways to define reverse : 'a list -> 'a list:
  one method consists of repeated applications of append to push
  the head of the list to the end, and the other only uses cons (::)
  (and is defined via an auxiliary function). Provide both these definitions
  and explain the difference between them.
**)


(* D *)
(**
  Define the function startsWith : 'a list -> 'a list -> bool which takes two
  lists as input and returns true if the first list is a prefix of the second.
  For example, startsWith [1;2] [1;2;3;4;5] should return true, but
  startsWith [1; 0; 2] [1; 2; 3; 4; 5] should return fasle. Explain the idea
  behind your implementation and provide execution traces for both examples above.

  Next, using startsWith, implement isSublist : 'a list -> 'a list -> bool
  which returns true if the second list contains the entirety of the first list
  (without breaking it into parts) inside it. For example, isSublist [2;3;4] [0;1;2;3;4;5]
  should return true while isSublist [2;3;4] [1;2;3;0;4;5] should return fasle.
  Explain your implementation.
**)


