open Combinators
let rec even_list_gen = fun s ->
  let (xccc10) = sizecheck s in
  match xccc10 with
  | true ->
      let (xccc11) = [] in
      let (xccc12) = int_gen () in
      let (xccc13) = double xccc12 in xccc13 :: xccc11
  | false ->
      let (size) = freq_gen s in
      let (base_case) =
        size ~base_case:
          (fun _ ->
             let (xccc14) = [] in
             let (xccc15) = int_gen () in
             let (xccc16) = double xccc15 in xccc16 :: xccc14) in
      let (recursive_case) =
        base_case ~recursive_case:
          (fun _ ->
             let (idx2ccc0) = s in
             let (idx5ccc0) = subs idx2ccc0 in
             let (idx17ccc3) = even_list_gen idx5ccc0 in
             let (idx1ccc0) = int_gen () in
             let (idx10ccc0) = double idx1ccc0 in idx10ccc0 :: idx17ccc3) in
      recursive_case