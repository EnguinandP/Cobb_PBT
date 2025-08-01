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
               let (base_case) =
                 size ~base_case:
                   (fun _ ->
                      let (idx2ccc2) = d in
                      let (idx393ccc0) = subs idx2ccc2 in
                      let (idx483) = size_bst_gen idx393ccc0 in
                      let (idx3ccc61) = lo in
                      let (idx484) = idx483 idx3ccc61 in
                      let (idx4ccc65) = hi in idx484 idx4ccc65) in
               let (recursive_case) =
                 base_case ~recursive_case:
                   (fun _ ->
                      let (idx2ccc2) = d in
                      let (idx393ccc0) = subs idx2ccc2 in
                      let (idx453) = size_bst_gen idx393ccc0 in
                      let (idx9) = x in
                      let (idx454) = idx453 idx9 in
                      let (idx4ccc58) = hi in
                      let (idx455ccc16) = idx454 idx4ccc58 in
                      let (idx480) = size_bst_gen idx393ccc0 in
                      let (idx3ccc60) = lo in
                      let (idx481) = idx480 idx3ccc60 in
                      let (idx482ccc23) = idx481 idx9 in
                      Node (idx9, idx482ccc23, idx455ccc16)) in
               recursive_case
           | false -> Leaf)