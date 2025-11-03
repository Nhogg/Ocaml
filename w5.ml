(* Week 5 exercises *)

let curry h x y = h (x, y)

let uncurry h p = let (x, y) = p in h  x y


