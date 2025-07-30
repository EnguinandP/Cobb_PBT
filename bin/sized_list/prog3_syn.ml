open Combinators
let rec sized_list_gen = fun s ->
  let (xccc3) = sizecheck s in
  match xccc3 with
  | true -> []
  | false ->
      let (size) = freq_gen s in
      let (base_case) = size ~base_case: (fun _ -> []) in
      let (recursive_case) =
        base_case ~recursive_case:
          (fun _ ->
             let (idx2ccc0) = s in
             let (idx6ccc0) = subs idx2ccc0 in
             let (idx18ccc2) = sized_list_gen idx6ccc0 in
             let (idx1ccc0) = int_gen () in idx1ccc0 :: idx18ccc2) in
      recursive_case