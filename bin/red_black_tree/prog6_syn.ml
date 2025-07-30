open Combinators
let rec rbtree_gen = fun inv ->
  fun color ->
    fun h ->
      let (xccc25) = sizecheck h in
      match xccc25 with
      | true ->
          (match color with
           | true -> Rbtleaf
           | false ->
               let (xccc26) = bool_gen () in
               (match xccc26 with
                | true -> Rbtleaf
                | false ->
                    let (xccc27) = Rbtleaf in
                    let (xccc28) = int_gen () in
                    let (xccc29) = Rbtleaf in
                    let (x_0) = true in Rbtnode (x_0, xccc29, xccc28, xccc27)))
      | false ->
          (match color with
           | true ->
               let (xccc30) = subs inv in
               let (xccc31) = rbtree_gen xccc30 in
               let (x_1) = false in
               let (xccc32) = xccc31 x_1 in
               let (xccc33) = subs h in
               let (lt2) = xccc32 xccc33 in
               let (xccc34) = subs inv in
               let (xccc35) = rbtree_gen xccc34 in
               let (x_2) = false in
               let (xccc36) = xccc35 x_2 in
               let (xccc37) = subs h in
               let (rt2) = xccc36 xccc37 in
               let (xccc38) = int_gen () in
               let (x_3) = false in Rbtnode (x_3, lt2, xccc38, rt2)
           | false ->
               let (fst_case) =
                 unif_gen
                   (fun _ ->
                      let (xccc39) = subs inv in
                      let (xccc40) = rbtree_gen xccc39 in
                      let (x_4) = true in
                      let (xccc41) = xccc40 x_4 in
                      let (lt3) = xccc41 h in
                      let (xccc42) = subs inv in
                      let (xccc43) = rbtree_gen xccc42 in
                      let (x_5) = true in
                      let (xccc44) = xccc43 x_5 in
                      let (rt3) = xccc44 h in
                      let (xccc45) = int_gen () in
                      let (x_6) = true in Rbtnode (x_6, lt3, xccc45, rt3)) in
               let (snd_case) =
                 fst_case
                   (fun _ ->
                      let (idx4ccc0) = inv in
                      let (idx68ccc0) = subs idx4ccc0 in
                      let (idx79) = rbtree_gen idx68ccc0 in
                      let (idx0ccc10) = true in
                      let (idx80) = idx79 idx0ccc10 in
                      let (idx6ccc8) = h in idx80 idx6ccc8) in
               snd_case)