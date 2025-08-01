open Combinators
let rec even_list_gen = fun s ->
  let (xccc6) = sizecheck s in
  match xccc6 with
  | true ->
      let (idx0ccc6) = [] in
      let (idx1ccc0) = int_gen () in
      let (idx14ccc0) = double idx1ccc0 in idx14ccc0 :: idx0ccc6
  | false ->
      let (size) = freq_gen s in
      let (base_case) =
        size ~base_case:
          (fun _ ->
             let (xccc7) = [] in
             let (xccc8) = int_gen () in
             let (xccc9) = double xccc8 in xccc9 :: xccc7) in
      let (recursive_case) =
        base_case ~recursive_case:
          (fun _ ->
             let (idx2ccc1) = s in
             let (idx7ccc0) = subs idx2ccc1 in
             let (idx29ccc3) = even_list_gen idx7ccc0 in
             let (idx1ccc1) = int_gen () in
             let (idx16ccc0) = double idx1ccc1 in idx16ccc0 :: idx29ccc3) in
      recursive_case