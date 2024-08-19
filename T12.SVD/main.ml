open Numerical

let () =
  let a1_matrix = Owl.Mat.of_array [| 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8. |] 4 2 in
  let u_matrix, sigma, v_t = Svd.svd_decomposition a1_matrix in

  Printf.printf
    "Em sequência, Matriz U, Sigma, V_t e reconstrução da matriz original";

  Owl.Mat.print u_matrix;
  Owl.Mat.print sigma;
  Owl.Mat.print v_t;

  Owl.Mat.print (Owl.Mat.dot (Owl.Mat.dot u_matrix sigma) v_t)
