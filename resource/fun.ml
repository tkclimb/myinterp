fun zero -> 0;
let f = let x = 2 in let g = fun y -> x + y in g in f 4;
