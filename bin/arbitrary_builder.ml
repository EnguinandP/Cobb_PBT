open Combinators

(* higher order programming *)
(* make still expects random state as parameter, "_" gets rid of it *)
let arb_builder f = QCheck.make (fun _ -> f ())

(* my generators *)
let int_list = arb_builder int_list_gen
let int_list_size = arb_builder int_list_variable_size_gen
let int_list_sorted = arb_builder int_list_sorted_gen
let int_list_dup = arb_builder int_list_dup_gen
let int_list_unique = arb_builder int_list_unique_gen


(* Cobb synthesized generators *)
let test_wrapper ()=
  let size = int_gen () in
  Sized_list_prog.sized_list_gen size

let test1 = arb_builder test_wrapper

