open Numerical
open Owl

let y x =
  let exp_neg_x = exp (-.x) in
  let exp_x = exp x in
  let numerator = exp_neg_x -. exp_x in
  let denominator = exp (-1.) -. exp 1. in
  numerator /. denominator

let t1 () =
  let partitions = 8 in
  let b = Array.init (partitions - 1) (fun _ -> 0.) in
  let b_vector = Mat.of_array b (partitions - 1) 1 in
  let result =
    Pvc.finit_differences_M1 (0., 1.) ~width:1. partitions b_vector
  in
  Mat.print result;

  Printf.printf "Valores exatos:\n";
  let dx = ref 0.125 in
  for i = 0 to 6 do
    let exact_value = y !dx in
    Printf.printf "R%d: %f " i exact_value;
    Printf.printf "Erro relativo: %f\n"
      (abs_float (Mat.get result i 0 -. exact_value) /. abs_float exact_value);
    dx := !dx +. 0.125
  done

let t2 () =
  let partitions = 8 in
  let total_length = (partitions - 1) * (partitions - 1) in
  let b = Array.init total_length (fun _ -> 4.) in
  let b_vector = Mat.of_array b total_length 1 in
  let result =
    Pvc.finit_differences_M2 (0., 0., 0., 0.) partitions ~width:1. ~height:1.
      b_vector
  in
  Mat.print result

let () =
  Printf.printf "Matriz 1 - Resultado\n";
  t1 ();
  Printf.printf "Matriz 2 - Resultado\n";
  t2 ()
