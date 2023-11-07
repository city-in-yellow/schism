(*
    APEX

    Roll n ten-sided dice, count values six and higher (k). 
        k > DC is good
        k > DC / 2 is okay
        otherwise bad
    
    We'll plot the chance of getting a particular DC by pool size.
*)

(* we want to output the rolls transposed *)
p is transpose ().

(* run on pool sizes 2-9 *)
for 2..=9 as idx do
    die is d idx 10.
    chance is sum (die >= 6).
    plot p chance "{idx}d10"


p = transpose();
for idx := range(2, 9):
    die = d(idx, 10);
    chance = sum(die >= 6);
    plot(p, chance, "{idx}d10")