open Numerical

let () =
  let a1_matrix =
    [|
      [| 40.; 8.; 4.; 2.; 1. |];
      [| 8.; 30.; 12.; 6.; 2. |];
      [| 4.; 12.; 20.; 1.; 2. |];
      [| 2.; 6.; 1.; 25.; 4. |];
      [| 1.; 2.; 2.; 4.; 5. |];
    |]
  in

  let a_bar, h_mat = Linear.householder_method a1_matrix in
  print_endline "Matriz nova (a barra):";
  Linear.print_matrix a_bar;
  print_endline "\nMatrix resultante H:";
  Linear.print_matrix h_mat;

  print_endline "\nAtenção, os autovetores abaixo não são os corretos ainda!";
  let init_vector = [| 1.; 0.; 0.; 0.; 0. |] in
  let ((lambda_1, _) as p1) =
    Linear.regular_power_iteration a_bar init_vector epsilon_float
  in
  let ((lambda_2, _) as p2) =
    Linear.inverse_power_iteration a_bar init_vector epsilon_float
  in
  let ((lambda_3, _) as p3) =
    Linear.shifted_power_iteration a_bar init_vector epsilon_float
      ((lambda_1 +. lambda_2) /. 2.)
  in
  let p4 =
    Linear.shifted_power_iteration a_bar init_vector epsilon_float
      ((lambda_3 +. lambda_2) /. 2.)
  in
  let p5 =
    Linear.shifted_power_iteration a_bar init_vector epsilon_float
      ((lambda_3 +. lambda_1) /. 2.)
  in
  Linear.print_eigen [ p1; p2; p3; p4; p5 ];

  print_endline "\nArrumando os autovetores usando a matriz h";

  let v1 = Linear.apply_matrix h_mat (snd p1)
  and v2 = Linear.apply_matrix h_mat (snd p2)
  and v3 = Linear.apply_matrix h_mat (snd p3)
  and v4 = Linear.apply_matrix h_mat (snd p4)
  and v5 = Linear.apply_matrix h_mat (snd p5) in
  List.iter (fun i -> Linear.print_vector i) [ v1; v2; v3; v4; v5 ]
