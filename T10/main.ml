open Numerical

(* Matrix must be symmetric and vector must be not null*)
let () =
  print_endline "Primeiro problema:";
  let a1_matrix = [| [| 5.; 2.; 1. |]; [| 2.; 3.; 1. |]; [| 1.; 1.; 2. |] |]
  and init_vector = [| 1.; 0.; 0. |] in
  let lamb, vec =
    Linear.regular_power_iteration a1_matrix init_vector epsilon_float
  in
  print_endline ("Autovalor: " ^ string_of_float lamb);
  print_string "Autovetor: ";
  Linear.print_vector vec;

  print_endline "Segundo problema:";
  let a2_matrix =
    [|
      [| 40.; 8.; 4.; 2.; 1. |];
      [| 8.; 30.; 12.; 6.; 2. |];
      [| 4.; 12.; 20.; 1.; 2. |];
      [| 2.; 6.; 1.; 25.; 4. |];
      [| 1.; 2.; 2.; 4.; 5. |];
    |]
  and init_vector = [| 1.; 0.; 0.; 0.; 0. |] in
  let lamb, vec =
    Linear.regular_power_iteration a2_matrix init_vector epsilon_float
  in
  print_endline ("Autovalor: " ^ string_of_float lamb);
  print_string "Autovetor: ";
  Linear.print_vector vec
