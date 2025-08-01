open Combinators
let rec even_list_gen = fun s ->
  let (xccc2) = sizecheck s in
  match xccc2 with
  | true ->
      let (idx0ccc9) = [] in
      let (idx1ccc1) = int_gen () in
      let (idx16ccc0) = double idx1ccc1 in idx16ccc0 :: idx0ccc9
  | false ->
      let (size) = freq_gen s in
      let (base_case) =
        size ~base_case:
          (fun _ ->
             let (idx2ccc0) = s in
             let (idx5ccc0) = subs idx2ccc0 in
             let (idx27ccc3) = even_list_gen idx5ccc0 in
             let (idx1ccc0) = int_gen () in
             let (idx14ccc0) = double idx1ccc0 in idx14ccc0 :: idx27ccc3) in
      let (recursive_case) =
        base_case ~recursive_case:
          (fun _ ->
             let (idx2ccc0) = s in
             let (idx5ccc0) = subs idx2ccc0 in even_list_gen idx5ccc0) in
      recursive_case