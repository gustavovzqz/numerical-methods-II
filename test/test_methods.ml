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
      Printf.printf "Produto interno de ";
      print_array v1;
      Printf.printf " e ";
      print_array v2;
      Printf.printf ": %f (esperado: %f)\n" result expected;
      assert (result = expected))
    test_cases

let () = test_dot_product ()
