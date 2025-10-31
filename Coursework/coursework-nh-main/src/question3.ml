(** Question 3 **)

(** a. (3 marks) **)

(* allDistinct : 'a list -> bool *)
(**
This function determines if a given list's elements
are all distinct. It takes a list and returns true or false.
To do this we first define an auxiliary function contains that
takes an element and a list. We pattern match on that list. If the list
if empty, we return false as there are no elements. Otherwise, we
check if the head of the list is equal to that the element. If it is,
then the list contains that element. Otherwise, we move forward and check
the next element. Outside the aux function, we do a simiar check
and see if the list contains two of the same elements. If it does, then
we return false. Otherwise, we continue checking other elements.
**)
let rec allDistinct list =
        let rec contains element list =
                match list with
                | [] -> false
                | h :: t -> if h = element then true else contains element t
        in
        match list with
        | [] -> true
        | h :: t ->
                        if contains h t then
                               false
                        else allDistinct t 

(** b. (2 marks) **)

(* allEqual : 'a list -> bool *)
(**
This function determines if all the elements in a list
are equal. It takes a list and returns either true or false.
We first match on the list. If it is empty, then we return true
as there are no elements and thus they are all equal. If it only
contains one element, we return true. Otherwise, we disect the
list and check if the first and second element are equal.
If they are, we recursively check the rest of the list. If they are not,
then we return false since there are two different elements in the list.
**)
let rec allEqual list =
        match list with
        | [] -> true
        | [x] -> true
        | h1 :: h2 :: t ->
                        if h1 = h2 then
                                allEqual (h2 :: t)
                        else false


(** c. (3 marks) **)

(* groupConsecutive : 'a list -> 'a list list *)
(**
This function takes a list and returns a list of lists
that contain all consecutive, equal elements grouped together.
To do this, we match on the list. If it is empty, we simply
return an empty list as there are no elements. If it contains
one element, we return a list containing that element.
Otherwise, we compare the first two elements. If they are equal,
we recursively group the the tail and merge the current element with
the head of the first group in the recusrive result.
If they are different, we start a new group with the current element and
continue grouping the rest of the list.
**)
let rec groupConsecutive list =
        match list with
        | [] -> []
        | [x] -> [[x]]
        | h1 :: h2 :: t ->
                        if h1 = h2 then
                                match groupConsecutive (h2 :: t) with
                                | [] -> [[h1]]
                                | (x :: xs) -> (h1 :: x) :: xs
                        else
                                [h1] :: groupConsecutive (h2 :: t)

(** d. (3 marks) **)

(* Grouping a list then flattening it returns the original sequence. *)
(* This function takes a list of lists and concatenates them into a single list *)
let rec flatten list =
        let rec append a b =
                match a with
                | [] -> b
                | h :: t -> h :: append t b
        in
        match list with
        | [] -> []
        | h :: t -> append h (flatten t)

(**
This predicate shows the property that grouping and flattening a list should
reproduce the original list. groupConsecutive cannot lose elements,
reorder elements, or create duplicates.
**)
let groupPred1 list =
        flatten (groupConsecutive list) = list

(* All lists returned by groupConsecutive are non empty and contain equal values. *)

(**
This function checks that every sublist in a list of lists is valid.
Each group must be non-empty and all elements within a group must be
equal.
**)
let rec allGroupsValid groups =
        match groups with
        | [] -> true
        | h :: t ->
                        (h <> [] && allEqual h) && allGroupsValid t

(**
This predicate shows the property that all groups created by groupConsecutive are
valid, meaning that no empty groups are created and all values in each
group are the same.
**)
let groupPred2 list =
        allGroupsValid (groupConsecutive list)

open QCheck

let groupPred1Test =
        Test.make
        ~name:"groupConsecutive keeps original list order and elements"
        (list int)
        (fun list -> groupPred1 list)

let groupPred2Test =
        Test.make
        ~name:"groupConsecutive groups are non-empty and contain equal values"
        (list int)
        (fun list -> groupPred2 list)

(** e. (2 marks) **)

(* allEqualWithWitness : 'a list -> 'a option * bool *)
(**
This function generalizes allEqlau by returning a boolean indicating
whether all elements in the list are equal and an optional "witness" value
that represents the repeated value.
We use foldLeft to traverse the list while maintaining key state 
information: acc_ok (boolean tracking whether all elements seen so far are
equal) and acc_val (the reference value that subsequent values are compared to.)
We first match the list. If it is empty, we return (None, true) as an empty list
satisfies this equality. Otherwise, we take the head value as the initial reference value
and fold to traverse the rest of the list. We compare each element (x) to acc_val.
If there is a mismatch, acc_ok becomes false and stays false. Finally, if ok is true, it
returns (Some value, true) otherwise (None, false.)
**)
let rec foldLeft v f xs =
        match xs with
        | [] -> v
        | x :: xs -> foldLeft (f v x) f xs

let allEqualWithWitness list =
        match list with
        | [] -> (None, true)
        | h :: t ->
                        let (ok, value) =
                                foldLeft (true, h)
                                (fun (acc_ok, acc_val) x ->
                                        (acc_ok && (x = acc_val), acc_val))
                                t
                        in
                        if ok then (Some value, true) else (None, false)

