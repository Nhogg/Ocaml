(* Week 3 exercises (lists) *)

let rec unzip xy = 
        match xy with
        | x :: y -> [x] :: [y]
        | [] -> []

let rec fromMtoN x y =
        if x > y then
                []
        else
                x :: fromMtoN ( x + 1 ) y

let upto n =
        fromMtoN 0 n

let rec take n xs = 
        
