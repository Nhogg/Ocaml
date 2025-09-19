

let transpose ((a, b), (c, d)) =
((a, c), (b, d))

let matrixMul (((a1, b1), (c1, d1)), ((a2, b2), (c2, d2))) =
(((a1 * a2 + b1 * c2, a1 * b2 + b1 * d2), (c1 * a2 + d1 * c2, c1 * b2 + d1 * d2)))


