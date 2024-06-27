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
  let q, r = Linear.qr_decomposition a_matrix in
  print_endline "Matriz Q:";
  Linear.print_matrix q;
  print_endline "\nMatrix R";
  Linear.print_matrix r
