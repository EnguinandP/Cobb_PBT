open Combinators
(* removed rec *)
let sized_list_gen (s : int) : int list =
  if sizecheck s then [] else if bool_gen () then [] else int_list_gen ()
