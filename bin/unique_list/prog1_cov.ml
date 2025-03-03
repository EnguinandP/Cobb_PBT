open Combinators
let rec unique_list_gen (s : int) : int list =
  if sizecheck s then int_list_size_gen s
  else
    let (l : int list) = unique_list_gen (subs s) in
    let (x : int) = int_gen () in
    if list_mem l x then raise BailOut else x :: l
