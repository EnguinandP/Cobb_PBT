open Combinators
let rec rbtree_gen = fun inv ->
  fun color ->
    fun h ->
      let (xccc27) = sizecheck h in
      match xccc27 with
      | true ->
          (match color with
           | true -> Rbtleaf
           | false ->
               let (xccc28) = bool_gen () in
               (match xccc28 with
                | true -> Rbtleaf
                | false ->
                    let (xccc29) = Rbtleaf in
                    let (xccc30) = int_gen () in
                    let (xccc31) = Rbtleaf in
                    let (x_0) = true in Rbtnode (x_0, xccc31, xccc30, xccc29)))
      | false ->
          (match color with
           | true ->
               let (idx4ccc0) = inv in
               let (idx68ccc0) = subs idx4ccc0 in
               let (idx157) = rbtree_gen idx68ccc0 in
               let (idx1ccc23) = false in
               let (idx158) = idx157 idx1ccc23 in
               let (idx6ccc0) = h in
               let (idx67ccc8) = subs idx6ccc0 in
               let (idx159ccc32) = idx158 idx67ccc8 in
               let (idx3ccc18) = int_gen () in
               let (idx159ccc31) = idx158 idx67ccc8 in
               let (idx1ccc0) = false in
               Rbtnode (idx1ccc0, idx159ccc31, idx3ccc18, idx159ccc32)
           | false ->
               let (fst_case) =
                 unif_gen
                   (fun _ ->
                      let (xccc32) = subs inv in
                      let (xccc33) = rbtree_gen xccc32 in
                      let (x_1) = true in
                      let (xccc34) = xccc33 x_1 in
                      let (lt3) = xccc34 h in
                      let (xccc35) = subs inv in
                      let (xccc36) = rbtree_gen xccc35 in
                      let (x_2) = true in
                      let (xccc37) = xccc36 x_2 in
                      let (rt3) = xccc37 h in
                      let (xccc38) = int_gen () in
                      let (x_3) = true in Rbtnode (x_3, lt3, xccc38, rt3)) in
               let (snd_case) =
                 fst_case
                   (fun _ ->
                      let (xccc39) = subs inv in
                      let (xccc40) = subs xccc39 in
                      let (xccc41) = rbtree_gen xccc40 in
                      let (x_4) = false in
                      let (xccc42) = xccc41 x_4 in
                      let (xccc43) = subs h in
                      let (lt4) = xccc42 xccc43 in
                      let (xccc44) = subs inv in
                      let (xccc45) = subs xccc44 in
                      let (xccc46) = rbtree_gen xccc45 in
                      let (x_5) = false in
                      let (xccc47) = xccc46 x_5 in
                      let (xccc48) = subs h in
                      let (rt4) = xccc47 xccc48 in
                      let (xccc49) = int_gen () in
                      let (x_6) = false in Rbtnode (x_6, lt4, xccc49, rt4)) in
               snd_case)