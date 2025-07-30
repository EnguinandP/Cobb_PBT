open Combinators
let rec depth_tree_gen = fun s ->
  let (xccc4) = sizecheck s in
  match xccc4 with
  | true -> Leaf
  | false ->
      let (size) = freq_gen s in
      let (base_case) = size ~base_case: (fun _ -> Leaf) in
      let (recursive_case) =
        base_case ~recursive_case:
          (fun _ ->
             let (ss) = subs s in
             let (lt) = depth_tree_gen ss in
             let (rt) = depth_tree_gen ss in
             let (n) = int_gen () in Node (n, lt, rt)) in
      recursive_case