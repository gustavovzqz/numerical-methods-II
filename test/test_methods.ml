open Numerical

let print_array arr =
  let n = Array.length arr in
  print_string "[";
  for i = 0 to n - 1 do
    Printf.printf "%g" arr.(i);
    if i < n - 1 then print_string "; "
  done;
  print_string "]";
  print_newline ()

let test_dot_product () =
  let test_cases =
    [
      ([| 1.0; 0.0; 0.0 |], [| 0.0; 1.0; 0.0 |], 0.0);
      ([| 3.0; 4.0 |], [| 6.0; 8.0 |], 50.0);
      ([| 2.0; 3.0; 6.0 |], [| 2.0; 3.0; 6.0 |], 49.0);
      ([| -1.0; -2.0 |], [| -3.0; -4.0 |], 11.0);
      ([| 1.5; 2.5; 3.5 |], [| 0.5; 1.5; 2.5 |], 13.25);
    ]
  in
  List.iter
    (fun (v1, v2, expected) ->
      let result = Linear.dot_product v1 v2 in
      assert (result = expected))
    test_cases

let test_normalize_vector () =
  let test_cases =
    [
      ([| 3.; 0.; 0. |], [| 1.; 0.; 0. |]); ([| 0.; 0.; 0. |], [| 0.; 0.; 0. |]);
    ]
  in
  List.iter
    (fun (v1, expected) ->
      print_array expected;
      let result = Linear.normalize_vector v1 in
      print_array result;
      assert (result = expected))
    test_cases

(* let test_solve_lower () =
   let a1_matrix =
     [| [| 1.; 0.; 0. |]; [| -3.; 2.; 0. |]; [| -1.; 1.; -1. |] |]
   in
   let b = [| 2.; -8.; 0. |] in
   print_array (Linear.solve_lower a1_matrix b) *)

(* let test_solve_upper () =
   let a1_matrix =
     [| [| 2.; -1.; 2. |]; [| 0.; 2.; -1. |]; [| 0.; 0.; 3. |] |]
   in
   let b = [| 7.; -3.; 3. |] in
   print_array (Linear.solve_upper a1_matrix b) *)

let test_lu () =
  let a1_matrix =
    [| [| -1.; 2.; -2. |]; [| 3.; -4.; 1. |]; [| 1.; -5.; 3. |] |]
  in
  let b = [| 6.; -11.; -10. |] in
  let l, u = Linear.lu_decomposition a1_matrix in
  print_array l.(0);
  print_array l.(1);
  print_array l.(2);
  print_endline "";
  print_array u.(0);
  print_array u.(1);
  print_array u.(2);

  let x = Linear.solve_lu l u b in
  print_array x

let () =
  test_dot_product () |> test_normalize_vector;
  print_endline "aa" |> test_lu
