(** Question 2 **)

(** a. (3 marks) **)

(* isValidPath : int -> int -> bool list -> bool *)
(**
This function takes the dimensions (m and n) of a grid and a boolean
list representing the path (p). It defines an auxiliary function to "walk"
the grid based on the path contained within p.
To do this, we first match on the contents of remainingPath.
If it is empty, we check if our coordinates are currently at the top right corner.
If they are, the path reaches the correct point and we return true. Otherwise,
our path was not correct and we return false.
Next, if there are items in remainingPath, we know there are more steps left
to take. If this si the case, we determine if it is true or false.
If it is true, we calculate if going up by one would push us outside of the grid.
If it does, then the path is not valid. Otherwise, we add 1 to the y coordinate
and call walk again with the new coordinates and the rest of the list.
This process repeats until we either go out of bounds or arrive at the top right
of the grid.
Example: isValidPath 2 2 [true; false; true; false] will return true
**)
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
(**
This function returns the coordinates passed through by a path.
It takes a boolean list and returns an (int * int) list. To do this,
we first define an auxiliary function, buildHistory, that takes the pair
(currentX, currentY), and a boolean list representing the remaining path
that has not been analyzed. We first match on the remaining path.
If the list is empty, we return the currentX and currentY as that is 
the only available coordinates we have. Otherwise, we define (nextX, nextY)
as the next coordinates reached by the current step in our path.
After that, we recursively call buildHistory on (nextX, nextY) and the
rest of the path to generate hte history of future coorinates. Finally,
we prepend the current position to the result of that recursive call,
which allows us to build the complete list of coordinates.
Example: history [true; false; true] will return
[(0,0); (0,1); (1,1); (1,2)]
**)
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
(**
This function determines the number of turns taken by a given path.
It takes a path and returns an integer.
To do this, we first match on the path. If the list is empty, there are
zero turns. If the list only has one move, then that path also contains zero
turns. Otherwise, we disect the list and determine how many unique transitions
occur within the list. For example, the list [true; false; true; false] contains
3 unique transitions. We calculate this by checking if h1 != h2. If so, then there
has been a unique transition. Otherwise, there was no turn. We then
add that result to the the result of the recursive call on the rest of the list.
**)
let rec turningNumber path =
        match path with
        | [] -> 0
        | [x] -> 0
        | h1 :: h2 :: t ->
                        let transitionCount = if h1 != h2 then 1 else 0 in
                        transitionCount + turningNumber (h2 :: t)

(** d. (3 marks) **)

(* allPaths : int -> int -> bool list list *)
(* Helper function to prepend a given move to every path in a list of paths. *)
let rec prependMove move paths =
        match paths with
        | [] -> []
        | h :: t -> (move :: h) :: (prependMove move t)

(**
This function computes all possible paths in an m-n grid.
It takes the dimensions (m and n) and returns a list of boolean
lists, where each boolean list represents a unique path from (0, 0) to
(m, n). To do this, we first define prependMove that adds a given move to
the front of every path in the list.
Next, we pattern match on the coordinates. If both are 0, the only path
is the empty list [[]]. Otherwise, we recursively compute paths that arrive
from above when m > 0 and those that arrive from the left when n > 0.
We then prepend true to the paths from above and false to the ones from the left.
Finally, we concatenate both results and return the complete list of
all possible paths.
**)        
let rec allPaths m n =
        match (m, n) with
        | (0, 0) -> [ [] ]
        | (m, n) ->
                        let upPaths =
                                if m > 0 then
                                        prependMove true (allPaths (m - 1) n)
                                else
                                        []
                        in
                        let rightPaths =
                                if n > 0 then
                                        prependMove false (allPaths m (n - 1))
                                else
                                        []
                        in
                        upPaths @ rightPaths
