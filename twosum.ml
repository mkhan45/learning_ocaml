module IntMap = Map.Make(Int)

let two_sum nums target =
    let rec aux nums target i m = match nums with
        | [] -> None
        | (x :: xs) -> match m |> IntMap.find_opt (target - x) with
            | Some found_index -> Some (i, found_index)
            | None -> aux xs target (i + 1) (m |> IntMap.add x i)
    in aux nums target 0 IntMap.empty
