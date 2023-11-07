(*
    Blades in the Dark

    Roll n six-sided dice, take highest. 
        1-3 is bad
        4-5 is good
        6 is great
    
    If multiple dice are 6 then you crit.
*)

fn blade die:
    (* take largest and second largest dice *)
    let first = highest die;
    let second = nth die 2;

    (* apply BitD ruleset *)
    if first = 6 and second = 6 then "crit"
    else first = 6 then "great"
    else first >= 4 then "okay"
    else "bad";

(* we want to output the rolls transposed *)
let p = transpose ();
(* run on pool sizes 1-7 *)
for 1..=7 -> idx:
    let res = blade (d idx 6);
    plot p res "{idx}d6"
