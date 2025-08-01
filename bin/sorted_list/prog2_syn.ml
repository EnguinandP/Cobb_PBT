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
             let (idx2ccc1) = s in
             let (idx15ccc0) = subs idx2ccc1 in
             let (idx209) = sorted_list_gen idx15ccc0 in
             let (idx7) = y in
             let (idx210ccc1) = idx209 idx7 in idx7 :: idx210ccc1
         | false -> raise BailOut)