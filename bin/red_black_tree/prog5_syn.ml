open Combinators
let rec rbtree_gen = fun inv ->
  fun color ->
    fun h ->
      let (xccc29) = sizecheck h in
      match xccc29 with
      | true ->
          (match color with
           | true -> Rbtleaf
           | false ->
               let (xccc30) = bool_gen () in
               (match xccc30 with
                | true -> Rbtleaf
                | false ->
                    let (xccc31) = Rbtleaf in
                    let (xccc32) = int_gen () in
                    let (xccc33) = Rbtleaf in
                    let (x_0) = true in Rbtnode (x_0, xccc33, xccc32, xccc31)))
      | false ->
          (match color with
           | true ->
               let (xccc34) = subs inv in
               let (xccc35) = rbtree_gen xccc34 in
               let (x_1) = false in
               let (xccc36) = xccc35 x_1 in
               let (xccc37) = subs h in
               let (lt2) = xccc36 xccc37 in
               let (xccc38) = subs inv in
               let (xccc39) = rbtree_gen xccc38 in
               let (x_2) = false in
               let (xccc40) = xccc39 x_2 in
               let (xccc41) = subs h in
               let (rt2) = xccc40 xccc41 in
               let (xccc42) = int_gen () in
               let (x_3) = false in Rbtnode (x_3, lt2, xccc42, rt2)
           | false ->
               let (fst_case) =
                 unif_gen
                   (fun _ ->
                      let (idx4ccc0) = inv in
                      let (idx68ccc0) = subs idx4ccc0 in
                      let (idx79) = rbtree_gen idx68ccc0 in
                      let (idx0ccc10) = true in
                      let (idx80) = idx79 idx0ccc10 in
                      let (idx6ccc8) = h in
                      let (idx81ccc24) = idx80 idx6ccc8 in
                      let (idx3ccc18) = int_gen () in
                      let (idx81ccc23) = idx80 idx6ccc8 in
                      let (idx0ccc0) = true in
                      Rbtnode (idx0ccc0, idx81ccc23, idx3ccc18, idx81ccc24)) in
               let (snd_case) =
                 fst_case
                   (fun _ ->
                      let (xccc43) = subs inv in
                      let (xccc44) = subs xccc43 in
                      let (xccc45) = rbtree_gen xccc44 in
                      let (x_4) = false in
                      let (xccc46) = xccc45 x_4 in
                      let (xccc47) = subs h in
                      let (lt4) = xccc46 xccc47 in
                      let (xccc48) = subs inv in
                      let (xccc49) = subs xccc48 in
                      let (xccc50) = rbtree_gen xccc49 in
                      let (x_5) = false in
                      let (xccc51) = xccc50 x_5 in
                      let (xccc52) = subs h in
                      let (rt4) = xccc51 xccc52 in
                      let (xccc53) = int_gen () in
                      let (x_6) = false in Rbtnode (x_6, lt4, xccc53, rt4)) in
               snd_case)