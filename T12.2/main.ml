open Numerical

let () =
  let a_matrix =
    [|
      [| 40.; 8.; 4.; 2.; 1. |];
      [| 8.; 30.; 12.; 6.; 2. |];
      [| 4.; 12.; 20.; 1.; 2. |];
      [| 2.; 6.; 1.; 25.; 4. |];
      [| 1.; 2.; 2.; 4.; 5. |];
    |]
  in
  let a_mat, p_mat = Linear.qr_method a_matrix epsilon_float in
  let p_mat = p_mat in
  print_endline "Matriz A:";
  Linear.print_matrix a_mat;
  print_endline "\nMatriz P";
  Linear.print_matrix p_mat;

  let p_mat = Linear.transpose p_mat in
  print_endline "\nPares Autovalor | Autovetor";
  let n = Array.length p_mat in
  for i = 0 to n - 1 do
    print_string "Autovalor: ";
    print_float a_mat.(i).(i);
    print_string " | Autovetor: ";
    Linear.print_vector p_mat.(i)
  done;

  (* Linear.qr_method_printing a_matrix epsilon_float *)
  print_endline "\nAgora, usando o método de Householder\n";
  Linear.print_matrix a_matrix;
  let a_bar, h_mat = Linear.householder_method a_matrix in
  print_endline "\nPrintando H: ";
  Linear.print_matrix h_mat;
  let a_mat, p_mat = Linear.qr_method a_bar epsilon_float in
  print_endline "\nMatriz A:";
  Linear.print_matrix a_mat;
  print_endline "\nMatriz P";
  Linear.print_matrix p_mat;

  print_endline "\nArrumando a matriz P fazendo H P";
  let p_mat = Linear.matrix_multiply h_mat p_mat in
  Linear.print_matrix p_mat;
  let p_mat = Linear.transpose p_mat in

  print_endline "\nPares Autovalor | Autovetor";
  let n = Array.length p_mat in
  for i = 0 to n - 1 do
    print_string "Autovalor: ";
    print_float a_mat.(i).(i);
    print_string " | Autovetor: ";
    Linear.print_vector p_mat.(i)
  done
