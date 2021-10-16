let x = ref 5 in
x := !x + 5;
x := !x * 2;
!x |> string_of_int |> print_endline;
