open Numerical

(* Matrix must be symmetric and vector must be not null*)
let () =
  let a1_matrix = [| [| 5.; 2.; 1. |]; [| 2.; 3.; 1. |]; [| 1.; 1.; 2. |] |]
  and random_vector = [| 1.; 0.; 0. |] in
  let lamb, vec =
    Linear.regular_power_iteration a1_matrix random_vector epsilon_float
  in
  print_endline (string_of_float lamb);
  Linear.print_vector vec
