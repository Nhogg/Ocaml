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
let history path = 
        let rec buildHistory (currentX, currentY) remainingPath =
                match remainingPath with
                | [] -> [(currentX, currentY)]
                | h :: t -> 
                                let (nextX, nextY) =
                                        if h = true then
                                                (currentX, currentY + 1)
                                        else
                                                (currentX + 1, currentY)
                                in
                                let futureHistory = buildHistory (nextX, nextY) t in
                                (currentX, currentY) :: futureHistory
        in buildHistory (0, 0) path







(** c. (3 marks) **)

(* turningNumber : bool list -> int *)
let rec turningNumber path =
        match path with
        | [] -> 0
        | [x] -> 0
        | h1 :: h2 :: t ->
                        let transitionCount = if h1 != h2 then 1 else 0 in
                        transitionCount + turningNumber (h2 :: t)

(** d. (3 marks) **)

(* allPaths : int -> int -> bool list list *)
let allPaths m n = 
        















