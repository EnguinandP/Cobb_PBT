open Combinators
let rec complete_tree_gen = fun s ->
  let (xccc2) = sizecheck s in
  match xccc2 with
  | true -> Leaf
  | false ->
      let (idx2ccc0) = s in
      let (idx7ccc0) = subs idx2ccc0 in
      let (idx19ccc8) = complete_tree_gen idx7ccc0 in
      let (idx19ccc7) = complete_tree_gen idx7ccc0 in
      let (idx1ccc0) = int_gen () in Node (idx1ccc0, idx19ccc7, idx19ccc8)