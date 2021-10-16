#use "lib.ml"

let euler_two = 
    let rec helper acc a b =
        if b < 4_000_000
            then helper (acc + b) b (a + 4 * b)
            else acc
    in helper 0 0 2;;

print_endline (string_of_int euler_two);;
