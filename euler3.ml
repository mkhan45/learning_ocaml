#use "lib.ml"

let euler_three = let rec n = 600851475143
                  and aux n = match largest_factor n with
                                    | Some f when n == f -> f
                                    | Some f -> aux f
                                    | None -> n
                    in aux n;;

euler_three |> string_of_int |> print_endline
