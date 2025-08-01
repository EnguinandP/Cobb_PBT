open Combinators
let rec duplicate_list_gen = fun s ->
  fun x ->
    let (xccc2) = sizecheck s in
    match xccc2 with
    | true -> []
    | false ->
        let (idx2ccc0) = s in
        let (idx7ccc0) = subs idx2ccc0 in
        let (idx42) = duplicate_list_gen idx7ccc0 in
        let (idx3ccc8) = x in
        let (idx43ccc1) = idx42 idx3ccc8 in
        let (idx3ccc0) = x in idx3ccc0 :: idx43ccc1