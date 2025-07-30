open Combinators
let rec even_list_gen = fun s ->
  let (xccc6) = sizecheck s in
  match xccc6 with
  | true ->
      let (xccc7) = [] in
      let (xccc8) = int_gen () in
      let (xccc9) = double xccc8 in xccc9 :: xccc7
  | false ->
      let (size) = freq_gen s in
      let (base_case) =
        size ~base_case:
          (fun _ ->
             let (idx0ccc3) = [] in
             let (idx1ccc0) = int_gen () in
             let (idx10ccc0) = double idx1ccc0 in idx10ccc0 :: idx0ccc3) in
      let (recursive_case) =
        base_case ~recursive_case:
          (fun _ ->
             let (idx2ccc0) = s in
             let (idx5ccc0) = subs idx2ccc0 in
             let (idx17ccc3) = even_list_gen idx5ccc0 in
             let (idx1ccc0) = int_gen () in
             let (idx10ccc0) = double idx1ccc0 in idx10ccc0 :: idx17ccc3) in
      recursive_case