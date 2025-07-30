open Combinators
let rec depth_tree_gen = fun s ->
  let (xccc3) = sizecheck s in
  match xccc3 with
  | true -> Leaf
  | false ->
      let (size) = freq_gen s in
      let (base_case) = size ~base_case: (fun _ -> Leaf) in
      let (recursive_case) =
        base_case ~recursive_case:
          (fun _ ->
             let (idx2ccc0) = s in
             let (idx6ccc0) = subs idx2ccc0 in
             let (idx10ccc8) = depth_tree_gen idx6ccc0 in
             let (idx10ccc7) = depth_tree_gen idx6ccc0 in
             let (idx1ccc0) = int_gen () in
             Node (idx1ccc0, idx10ccc7, idx10ccc8)) in
      recursive_case