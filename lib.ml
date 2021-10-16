let (--) l r =
    let rec range acc l r = if (l == r)
                                then acc
                                else range (r :: acc) l (r - 1)
    in range [] l r;;

let sum ls = List.fold_left (+) 0 ls;;

let factor = function
    | 1 -> None
    | n -> let smallest_factor = List.find_opt (fun d -> n mod d = 0) (2--(n/2 + 1))
            in match smallest_factor with
             | Some f -> Some (n / f)
             | None -> None

let rec gcd a b = match a, b with
    | 0, b -> b
    | a, 0 -> a
    | a, b when a > b -> gcd b a
    | a, b -> let remainder = b mod a
                in (if remainder != 0 then (gcd a remainder) else a)

let pollard_factor = function
    | 1 -> None
    | n when n mod 2 = 0 -> Some 2
    | n -> let g x n = (x * x + 1) mod n
            in let rec iter x y d = match x, y, d with
                   | x, y, 1 -> let x = g x n
                                and y = g (g y n) n in
                                let d = gcd (abs (x - y)) n
                                  in iter x y d
                   | _, _, d when d = n -> None
                   | _, _, d -> Some d
                in iter 2 2 1

let largest_factor n = let d = pollard_factor n
                        in Option.map (fun d -> n / d) d
