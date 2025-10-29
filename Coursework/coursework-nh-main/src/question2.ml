(** Question 2 **)

(** a. (3 marks) **)

(* isValidPath : int -> int -> bool list -> bool *)
let isValidPath m n p =
        let rec walk (currentX, currentY) remainingPath =
                match remainingPath with
                | [] -> 
                        if currentX = n  && currentY = m  then 
                                true 
                        else
                                false
                | h :: t -> 
                        if h = true then
                                let newY = currentY + 1 in 
                                if newY <= m then 
                                        walk (currentX, newY) t 
                                else 
                                        false 
                        else
                                let newX = currentX + 1 in
                                if newX <= n then 
                                        walk (newX, currentY) t 
                                else 
                                        false
        in walk (0, 0) p

(** b. (3 marks) **)

(* history : bool list -> (int * int) list *)
let history = failwith "Not implemented"

(** c. (3 marks) **)

(* turningNumber : bool list -> int *)
let turningNumber = failwith "Not implemented"

(** d. (3 marks) **)

(* allPaths : int -> int -> bool list list *)
let allPaths = failwith "Not implemented"
