open Combinators
let rec sorted_list_gen = fun s ->
  fun x ->
    let (xccc3) = sizecheck s in
    match xccc3 with
    | true -> []
    | false ->
        let (y) = int_gen () in
        let (xccc4) = x <= y in
        (match xccc4 with
         | true ->
             let (idx2ccc0) = s in
             let (idx11ccc0) = subs idx2ccc0 in
             let (idx97) = sorted_list_gen idx11ccc0 in
             let (idx7) = y in
             let (idx98ccc1) = idx97 idx7 in idx7 :: idx98ccc1
         | false -> raise BailOut)