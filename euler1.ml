#use "lib.ml"

let euler_one =
    sum (List.filter (fun x -> x mod 3 == 0 || x mod 5 == 0) (0--1000));;

print_endline (string_of_int euler_one);;
