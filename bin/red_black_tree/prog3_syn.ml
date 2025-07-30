open Combinators
let rec rbtree_gen = fun inv ->
  fun color ->
    fun h ->
      let (xccc33) = sizecheck h in
      match xccc33 with
      | true ->
          (match color with
           | true -> Rbtleaf
           | false ->
               let (idx7) = bool_gen () in
               (match idx7 with
                | true -> Rbtleaf
                | false ->
                    let (idx2ccc12) = Rbtleaf in
                    let (idx3ccc10) = int_gen () in
                    let (idx2ccc11) = Rbtleaf in
                    let (idx0ccc0) = true in
                    Rbtnode (idx0ccc0, idx2ccc11, idx3ccc10, idx2ccc12)))
      | false ->
          (match color with
           | true ->
               let (xccc35) = subs inv in
               let (xccc36) = rbtree_gen xccc35 in
               let (x_0) = false in
               let (xccc37) = xccc36 x_0 in
               let (xccc38) = subs h in
               let (lt2) = xccc37 xccc38 in
               let (xccc39) = subs inv in
               let (xccc40) = rbtree_gen xccc39 in
               let (x_1) = false in
               let (xccc41) = xccc40 x_1 in
               let (xccc42) = subs h in
               let (rt2) = xccc41 xccc42 in
               let (xccc43) = int_gen () in
               let (x_2) = false in Rbtnode (x_2, lt2, xccc43, rt2)
           | false ->
               let (fst_case) =
                 unif_gen
                   (fun _ ->
                      let (xccc44) = subs inv in
                      let (xccc45) = rbtree_gen xccc44 in
                      let (x_3) = true in
                      let (xccc46) = xccc45 x_3 in
                      let (lt3) = xccc46 h in
                      let (xccc47) = subs inv in
                      let (xccc48) = rbtree_gen xccc47 in
                      let (x_4) = true in
                      let (xccc49) = xccc48 x_4 in
                      let (rt3) = xccc49 h in
                      let (xccc50) = int_gen () in
                      let (x_5) = true in Rbtnode (x_5, lt3, xccc50, rt3)) in
               let (snd_case) =
                 fst_case
                   (fun _ ->
                      let (xccc51) = subs inv in
                      let (xccc52) = subs xccc51 in
                      let (xccc53) = rbtree_gen xccc52 in
                      let (x_6) = false in
                      let (xccc54) = xccc53 x_6 in
                      let (xccc55) = subs h in
                      let (lt4) = xccc54 xccc55 in
                      let (xccc56) = subs inv in
                      let (xccc57) = subs xccc56 in
                      let (xccc58) = rbtree_gen xccc57 in
                      let (x_7) = false in
                      let (xccc59) = xccc58 x_7 in
                      let (xccc60) = subs h in
                      let (rt4) = xccc59 xccc60 in
                      let (xccc61) = int_gen () in
                      let (x_8) = false in Rbtnode (x_8, lt4, xccc61, rt4)) in
               snd_case)