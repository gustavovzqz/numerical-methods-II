open Numerical

let () =
  print_endline "Primeiro problema:";
  let a1_matrix = [| [| 5.; 2.; 1. |]; [| 2.; 3.; 1. |]; [| 1.; 1.; 2. |] |]
  and init_vector = [| 1.; 0.; 0. |] in
  let ((lambda_1, _) as p1) =
    Linear.regular_power_iteration a1_matrix init_vector epsilon_float
  in
  let ((lambda_2, _) as p2) =
    Linear.inverse_power_iteration a1_matrix init_vector epsilon_float
  in
  let p3 =
    Linear.shifted_power_iteration a1_matrix init_vector epsilon_float
      ((lambda_1 +. lambda_2) /. 2.)
  in
  Linear.print_eigen [ p1; p2; p3 ];

  print_endline "Segundo problema:";
  let a2_matrix =
    [| [| -14.; 1.; -2. |]; [| 1.; -1.; 1. |]; [| -2.; 1.; -11. |] |]
  and init_vector = [| 1.; 0.; 0. |] in
  let ((lambda_1, _) as p1) =
    Linear.regular_power_iteration a2_matrix init_vector epsilon_float
  in
  let ((lambda_2, _) as p2) =
    Linear.inverse_power_iteration a2_matrix init_vector epsilon_float
  in
  let p3 =
    Linear.shifted_power_iteration a2_matrix init_vector epsilon_float
      ((lambda_1 +. lambda_2) /. 2.)
  in
  Linear.print_eigen [ p1; p2; p3 ];

  print_endline "Terceiro problema:";
  let a3_matrix =
    [|
      [| 40.; 8.; 4.; 2.; 1. |];
      [| 8.; 30.; 12.; 6.; 2. |];
      [| 4.; 12.; 20.; 1.; 2. |];
      [| 2.; 6.; 1.; 25.; 4. |];
      [| 1.; 2.; 2.; 4.; 5. |];
    |]
  and init_vector = [| 1.; 0.; 0.; 0.; 0. |] in
  let ((lambda_1, _) as p1) =
    Linear.regular_power_iteration a3_matrix init_vector epsilon_float
  in
  let ((lambda_2, _) as p2) =
    Linear.inverse_power_iteration a3_matrix init_vector epsilon_float
  in
  let ((lambda_3, _) as p3) =
    Linear.shifted_power_iteration a3_matrix init_vector epsilon_float
      ((lambda_1 +. lambda_2) /. 2.)
  in
  let p4 =
    Linear.shifted_power_iteration a3_matrix init_vector epsilon_float
      ((lambda_3 +. lambda_2) /. 2.)
  in
  let p5 =
    Linear.shifted_power_iteration a3_matrix init_vector epsilon_float
      ((lambda_3 +. lambda_1) /. 2.)
  in
  Linear.print_eigen [ p1; p2; p3; p4; p5 ]
