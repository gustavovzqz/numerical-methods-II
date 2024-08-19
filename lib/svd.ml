open Owl

let svd_decomposition_RC a_matrix =
  let a_transpose = Mat.transpose a_matrix in
  let a_bar = Mat.dot a_matrix a_transpose in

  let eigvec_C, eigval_C = Linalg.D.eig a_bar in

  let u_matrix = Owl_dense_matrix_z.re eigvec_C
  and eigval_R = Owl_dense_matrix_z.re eigval_C in

  let singular_values = Mat.map (fun i -> sqrt i) eigval_R in
  let sigma_matrix = Mat.diagm singular_values in

  let v_matrix =
    Mat.dot (Mat.dot a_transpose u_matrix) (Mat.inv sigma_matrix)
  in

  (u_matrix, sigma_matrix, Mat.transpose v_matrix)

let svd_decomposition_CR a_matrix =
  let a_transpose = Mat.transpose a_matrix in

  let a_bar = Mat.dot a_transpose a_matrix in

  let eigvec_C, eigval_C = Linalg.D.eig a_bar in

  let v_matrix = Owl_dense_matrix_z.re eigvec_C
  and eigval_R = Owl_dense_matrix_z.re eigval_C in

  let singular_values = Mat.map (fun i -> sqrt i) eigval_R in
  let sigma_matrix = Mat.diagm singular_values in

  let u_matrix = Mat.dot (Mat.dot a_matrix v_matrix) (Mat.inv sigma_matrix) in

  (u_matrix, sigma_matrix, Mat.transpose v_matrix)

let svd_decomposition a_matrix =
  let rows = Mat.row_num a_matrix and columns = Mat.col_num a_matrix in

  if rows < columns then svd_decomposition_RC a_matrix
  else svd_decomposition_CR a_matrix
