open Combinators
let rec rbtree_gen = fun inv ->
  fun color ->
    fun h ->
      let (xccc37) = sizecheck h in
      match xccc37 with
      | true ->
          (match color with
           | true -> Rbtleaf
           | false ->
               let (xccc38) = bool_gen () in
               (match xccc38 with
                | true -> Rbtleaf
                | false ->
                    let (xccc39) = Rbtleaf in
                    let (xccc40) = int_gen () in
                    let (xccc41) = Rbtleaf in
                    let (x_0) = true in Rbtnode (x_0, xccc41, xccc40, xccc39)))
      | false ->
          (match color with
           | true ->
               let (xccc42) = subs inv in
               let (xccc43) = rbtree_gen xccc42 in
               let (x_1) = false in
               let (xccc44) = xccc43 x_1 in
               let (xccc45) = subs h in
               let (lt2) = xccc44 xccc45 in
               let (xccc46) = subs inv in
               let (xccc47) = rbtree_gen xccc46 in
               let (x_2) = false in
               let (xccc48) = xccc47 x_2 in
               let (xccc49) = subs h in
               let (rt2) = xccc48 xccc49 in
               let (xccc50) = int_gen () in
               let (x_3) = false in Rbtnode (x_3, lt2, xccc50, rt2)
           | false ->
               let (fst_case) =
                 unif_gen
                   (fun _ ->
                      let (xccc51) = subs inv in
                      let (xccc52) = rbtree_gen xccc51 in
                      let (x_4) = true in
                      let (xccc53) = xccc52 x_4 in
                      let (lt3) = xccc53 h in
                      let (xccc54) = subs inv in
                      let (xccc55) = rbtree_gen xccc54 in
                      let (x_5) = true in
                      let (xccc56) = xccc55 x_5 in
                      let (rt3) = xccc56 h in
                      let (xccc57) = int_gen () in
                      let (x_6) = true in Rbtnode (x_6, lt3, xccc57, rt3)) in
               let (snd_case) =
                 fst_case
                   (fun _ ->
                      let (xccc58) = subs inv in
                      let (xccc59) = subs xccc58 in
                      let (xccc60) = rbtree_gen xccc59 in
                      let (x_7) = false in
                      let (xccc61) = xccc60 x_7 in
                      let (xccc62) = subs h in
                      let (lt4) = xccc61 xccc62 in
                      let (xccc63) = subs inv in
                      let (xccc64) = subs xccc63 in
                      let (xccc65) = rbtree_gen xccc64 in
                      let (x_8) = false in
                      let (xccc66) = xccc65 x_8 in
                      let (xccc67) = subs h in
                      let (rt4) = xccc66 xccc67 in
                      let (xccc68) = int_gen () in
                      let (x_9) = false in Rbtnode (x_9, lt4, xccc68, rt4)) in
               snd_case)