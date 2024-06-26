type vector = float array
type matrix = float array array

let add_vectors v1 v2 =
  let n1 = Array.length v1 and n2 = Array.length v2 in
  assert (n1 = n2);
  Array.init n1 (fun i -> v1.(i) +. v2.(i))

let scale_vector k vec =
  let n = Array.length vec in
  Array.init n (fun i -> k *. vec.(i))

let identity_matrix n =
  Array.init n (fun i -> Array.init n (fun j -> if i <> j then 0. else 1.))

let init_vec n = Array.make n 0.

let vector_norm vec =
  let sum_of_squares = Array.fold_left (fun acc x -> acc +. (x *. x)) 0. vec in
  sqrt sum_of_squares

let normalize_vector vec =
  let scale_factor = vector_norm vec in
  if scale_factor = 0. then vec else scale_vector (1. /. scale_factor) vec

let dot_product v1 v2 =
  let n1 = Array.length v1 and n2 = Array.length v2 in
  assert (n1 = n2);

  let rec dot_prod (iter : int) (acc : float) : float =
    if iter = n1 then acc
    else dot_prod (iter + 1) (acc +. (v1.(iter) *. v2.(iter)))
  in

  dot_prod 0 0.

let sub_matrix m1 m2 =
  let n = Array.length m1 in
  Array.init n (fun i -> Array.init n (fun j -> m1.(i).(j) -. m2.(i).(j)))

let scale_matrix lambda mat =
  let n = Array.length mat in
  Array.init n (fun i -> Array.init n (fun j -> mat.(i).(j) *. lambda))

let sub_vector v1 v2 =
  let n = Array.length v1 in
  Array.init n (fun i -> v1.(i) -. v2.(i))

let prod_vec v1 v2 =
  let n = Array.length v1 in
  Array.init n (fun i -> Array.init n (fun j -> v1.(i) *. v2.(j)))

let apply_matrix mat vec =
  let columns = Array.length mat and n = Array.length vec in
  assert (n = columns);
  Array.init n (fun i -> dot_product mat.(i) vec)

let matrix_copy mat =
  Array.init (Array.length mat) (fun i -> Array.copy mat.(i))

let transpose matrix =
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in
  Array.init cols (fun col -> Array.init rows (fun row -> matrix.(row).(col)))

let matrix_multiply matrix1 matrix2 =
  let rows1 = Array.length matrix1 in
  let cols1 = Array.length matrix1.(0) in
  let cols2 = Array.length matrix2.(0) in
  let transposed_matrix2 = transpose matrix2 in
  Array.init rows1 (fun row ->
      Array.init cols2 (fun col ->
          let dot_product = ref 0. in
          for i = 0 to cols1 - 1 do
            dot_product :=
              !dot_product +. (matrix1.(row).(i) *. transposed_matrix2.(col).(i))
          done;
          !dot_product))

let print_vector arr =
  let n = Array.length arr in
  print_string "[";
  for i = 0 to n - 1 do
    Printf.printf "%g" arr.(i);
    if i < n - 1 then print_string "; "
  done;
  print_string "]";
  print_newline ()

let print_matrix mat = Array.iter (fun i -> print_vector i) mat

let print_eigen lambda_list =
  let print_pair (lamb, vec) =
    print_string ("Autovalor: " ^ string_of_float lamb);
    print_string " | Autovetor: ";
    print_vector vec
  in
  let rec print_lambdas lambda_l =
    match lambda_l with
    | [] -> ()
    | h :: t ->
        print_pair h;
        print_lambdas t
  in
  print_lambdas lambda_list

let regular_power_iteration mat vec epsilon =
  let rec power_iteration new_lambda new_vec_k =
    let old_lambda = new_lambda in
    let old_vec_k = new_vec_k in
    let old_x1 = normalize_vector old_vec_k in
    let new_vec_k = apply_matrix mat old_x1 in
    let new_lambda = dot_product old_x1 new_vec_k in
    let error = abs_float ((new_lambda -. old_lambda) /. new_lambda) in
    if error > epsilon then power_iteration new_lambda new_vec_k
    else (new_lambda, old_x1)
  in
  power_iteration 0. vec

let lu_decomposition mat =
  let n = Array.length mat in
  let u = matrix_copy mat in
  let l = identity_matrix n in

  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      l.(j).(i) <- u.(j).(i) /. u.(i).(i);
      for k = i to n - 1 do
        u.(j).(k) <- u.(j).(k) -. (l.(j).(i) *. u.(i).(k))
      done
    done
  done;
  (l, u)

let solve_lower lmatrix b =
  let n = Array.length lmatrix in
  assert (Array.length b = n);
  let x_array = Array.copy b in
  for i = 0 to n - 1 do
    for j = 0 to i - 1 do
      x_array.(i) <- x_array.(i) -. (lmatrix.(i).(j) *. x_array.(j))
    done;
    x_array.(i) <- x_array.(i) /. lmatrix.(i).(i)
  done;
  x_array

let solve_upper lmatrix b =
  let n = Array.length lmatrix in
  assert (Array.length b = n);
  let x_array = Array.copy b in
  for i = n - 1 downto 0 do
    for j = n - 1 downto i + 1 do
      x_array.(i) <- x_array.(i) -. (lmatrix.(i).(j) *. x_array.(j))
    done;
    x_array.(i) <- x_array.(i) /. lmatrix.(i).(i)
  done;
  x_array

let solve_lu l_mat u_mat b_vec =
  let y_vec = solve_lower l_mat b_vec in
  solve_upper u_mat y_vec

let inverse_power_iteration mat vec epsilon =
  let l_matrix, u_matrix = lu_decomposition mat in
  let rec inverse_iteration new_lambda new_vec_k =
    let old_lambda = new_lambda in
    let old_vec_k = new_vec_k in
    let old_x1 = normalize_vector old_vec_k in
    let new_vec_k = solve_lu l_matrix u_matrix old_x1 in
    let new_lambda = dot_product old_x1 new_vec_k in
    let error = abs_float ((new_lambda -. old_lambda) /. new_lambda) in
    if error > epsilon then inverse_iteration new_lambda new_vec_k
    else (1. /. new_lambda, old_x1)
  in
  inverse_iteration 0. vec

let shifted_power_iteration mat vec epsilon mi =
  let id_matrix = identity_matrix (Array.length mat) in
  let shifted_mat = sub_matrix mat (scale_matrix mi id_matrix) in
  let lambda, eigen_vec = inverse_power_iteration shifted_mat vec epsilon in
  (lambda +. mi, eigen_vec)

let create_new_householder_matrix a_mat last_column =
  let mat_lenght = Array.length a_mat in
  let vec_lenght = Array.length a_mat.(0) in
  assert (vec_lenght = mat_lenght);
  let n = mat_lenght in
  let id_matrix = identity_matrix mat_lenght in
  let w_vec = init_vec vec_lenght in
  let w_line_vec = init_vec vec_lenght in
  for i = last_column + 1 to n - 1 do
    w_vec.(i) <- a_mat.(i).(last_column)
  done;
  w_line_vec.(last_column + 1) <- vector_norm w_vec;
  let n_vector = normalize_vector (sub_vector w_vec w_line_vec) in
  sub_matrix id_matrix (scale_matrix 2. (prod_vec n_vector n_vector))

let householder_method a_matrix =
  let n = Array.length a_matrix in
  let id_matrix = identity_matrix n in
  let rec hh_method a_old h iter =
    let h_i = create_new_householder_matrix a_old iter in
    let a_i = matrix_multiply (matrix_multiply h_i a_old) h_i in
    let hh = matrix_multiply h h_i in
    if iter = n - 3 then (a_i, hh) else hh_method a_i hh (iter + 1)
  in
  hh_method a_matrix id_matrix 0

let qr_decomposition a_mat =
  let a_old = ref (matrix_copy a_mat) in
  let m = Array.length a_mat in
  let n = Array.length a_mat.(0) in
  let q_mat = ref (identity_matrix n) in
  for j = 0 to n - 2 do
    for i = j + 1 to n - 1 do
      let q = identity_matrix m in
      let x = !a_old.(j).(j) in
      let y = !a_old.(i).(j) in
      let l = sqrt ((x *. x) +. (y *. y)) in
      let c = x /. l in
      let s = -.(y /. l) in
      q.(i).(i) <- c;
      q.(j).(j) <- c;
      q.(i).(j) <- s;
      q.(j).(i) <- -.s;
      q_mat := matrix_multiply !q_mat (transpose q);
      a_old := matrix_multiply q !a_old
    done
  done;
  (!q_mat, !a_old)

let test_diagonal_matrix a_mat =
  let n = Array.length a_mat in
  let s = ref 0. in
  for j = 0 to n - 2 do
    for i = j + 1 to n - 1 do
      s := !s +. (a_mat.(i).(j) *. a_mat.(i).(j))
    done
  done;
  sqrt !s

let qr_method a_mat epsilon =
  let n = Array.length a_mat in
  let rec qr_iteration old_a_mat old_p_mat =
    let q_mat, r_mat = qr_decomposition old_a_mat in
    let new_a_mat = matrix_multiply r_mat q_mat in
    let new_p_mat = matrix_multiply old_p_mat q_mat in
    let err_value = test_diagonal_matrix new_a_mat in
    if err_value < epsilon then (new_a_mat, new_p_mat)
    else qr_iteration new_a_mat new_p_mat
  in
  qr_iteration a_mat (identity_matrix n)

let qr_method_printing a_mat epsilon =
  let n = Array.length a_mat in
  let rec qr_iteration old_a_mat old_p_mat =
    print_matrix old_a_mat;
    print_endline "";
    let q_mat, r_mat = qr_decomposition old_a_mat in
    let new_a_mat = matrix_multiply r_mat q_mat in
    let new_p_mat = matrix_multiply old_p_mat q_mat in
    let err_value = test_diagonal_matrix new_a_mat in
    if err_value < epsilon then () else qr_iteration new_a_mat new_p_mat
  in
  qr_iteration a_mat (identity_matrix n)

let qr_householder a_mat h_mat epsilon =
  let rec qr_iteration old_a_mat old_x_mat =
    let q_mat, r_mat = qr_decomposition old_a_mat in
    let new_a_mat = matrix_multiply r_mat q_mat in
    let new_x_mat = matrix_multiply old_x_mat q_mat in
    let err_value = test_diagonal_matrix new_a_mat in
    if err_value < epsilon then (new_a_mat, new_x_mat)
    else qr_iteration new_a_mat new_x_mat
  in
  qr_iteration a_mat h_mat
