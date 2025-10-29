(** Question 1 **)

(** a. (2 marks) **)

(* f1 : 'b -> 'a -> ('a -> 'b -> 'c) -> 'c *)
let f1 a b func =
        func a b


(* f2 : ('a -> 'a) list -> 'a -> 'a *)
let rec f2 list startVal =
        match list with
        | [] -> startVal
        | a :: b -> f2 b (a :: startVal)

(** b. (3 marks) **)

(* Provide your answer in this comment block.
f1: We are given three arguments: two generic values ('a and 'b) and a function that
    takes a and b and returns a value of type 'c. According to the signature, the
    only logical step is to apply the function to 'a and 'b as that returns 'c.

f2: We are given a list of functions and a generic value ('a or startVal). This implies that
    we must apply each function in the list to some value, starting from startVal.
    To do this, we use a match condition. If the list of functions is empty, we simply
    return the startVal. Otherwise, if the list matches a :: b, we apply the head function
    (a) to the startVal, and then recusrively call f2 with the tail of the list (b) and the
    new result.
*)
