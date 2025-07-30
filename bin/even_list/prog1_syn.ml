open Combinators
let rec even_list_gen = fun s ->
  let (xccc12) = sizecheck s in
  match xccc12 with
  | true ->
      let (idx0ccc3) = [] in
      let (idx1ccc0) = int_gen () in
      let (idx11ccc0) = double idx1ccc0 in idx11ccc0 :: idx0ccc3
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
             let (xccc17) = subs s in
             let (xccc18) = even_list_gen xccc17 in
             let (xccc19) = int_gen () in
             let (xccc20) = double xccc19 in xccc20 :: xccc18) in
      recursive_case