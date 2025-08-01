open Combinators
let rec size_bst_gen = fun d ->
  fun lo ->
    fun hi ->
      let (xccc6) = sizecheck d in
      match xccc6 with
      | true -> Leaf
      | false ->
          let (xccc7) = incr lo in
          let (xccc8) = xccc7 < hi in
          (match xccc8 with
           | true ->
               let (xccc9) = int_range lo in
               let (x) = xccc9 hi in
               let (size) = freq_gen d in
               let (base_case) = size ~base_case: (fun _ -> Leaf) in
               let (recursive_case) =
                 base_case ~recursive_case:
                   (fun _ ->
                      let (idx2ccc0) = d in
                      let (idx393ccc0) = subs idx2ccc0 in
                      let (idx453) = size_bst_gen idx393ccc0 in
                      let (idx9) = x in
                      let (idx454) = idx453 idx9 in
                      let (idx4ccc56) = hi in
                      let (idx455ccc17) = idx454 idx4ccc56 in
                      let (idx480) = size_bst_gen idx393ccc0 in
                      let (idx3ccc58) = lo in
                      let (idx481) = idx480 idx3ccc58 in
                      let (idx482ccc25) = idx481 idx9 in
                      Node (idx9, idx482ccc25, idx455ccc17)) in
               recursive_case
           | false -> Leaf)