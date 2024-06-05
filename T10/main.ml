open Numerical

let regular_power_iteration (mat : Linear.matrix) (vec : Linear.vector)
    (epsilon : float) =
  let rec power_iteration new_lambda new_vec_k =
    print_endline (string_of_float new_lambda);
    let old_lambda = new_lambda in
    let old_vec_k = new_vec_k in
    let old_x1 = Linear.normalize_vector old_vec_k in
    let new_vec_k = Linear.apply_matrix mat old_x1 in
    let new_lambda = Linear.dot_product old_x1 new_vec_k in
    let error = abs_float ((new_lambda -. old_lambda) /. new_lambda) in
    if error > epsilon then power_iteration new_lambda new_vec_k
    else (new_lambda, old_x1)
  in

  power_iteration 0. vec

let () =
  let a1_matrix = [| [| 5.; 2.; 1. |]; [| 2.; 3.; 1. |]; [| 1.; 1.; 2. |] |]
  and random_vector = [| 0.; 0.; 0. |] in
  let lamb, _ = regular_power_iteration a1_matrix random_vector 0.3 in
  print_endline (string_of_float lamb)
