

let transpose ((a, b), (c, d)) =
((a, c), (b, d))

let matrixMul (((a1, b1), (c1, d1)), ((a2, b2), (c2, d2))) =
(((a1 * a2 + b1 * c2, a1 * b2 + b1 * d2), (c1 * a2 + d1 * c2, c1 * b2 + d1 * d2)))

type matrix = (int * int) * (int * int)

let rec matrixPow matrix pow = if pow = 0 then
        ((1, 0), (0, 1)) else
        matrixMul (matrix, matrixPow matrix (pow - 1))

let rec fastMatrixPow matrix pow = if pow = 0 then
        ((1, 0), (0, 1)) else if pow mod 2 = 0 then
                let half = fastMatrixPow matrix (pow / 2) in
                matrixMul (half, half)
        else
                matrixMul (matrix, fastMatrixPow matrix (pow - 1))

let rec fasterFib n = 
        let ((_, fn), _) = fastMatrixPow ((1, 1), (1, 0)) n in
        fn

let time f x = 
        let t = Sys.time() in
        let fx = f x in
        Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
        fx
