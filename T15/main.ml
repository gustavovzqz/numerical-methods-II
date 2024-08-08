open Numerical
open Owl

let t1 () =
  let partitions = 4 in
  let total_length = (partitions - 1) * (partitions - 1) in
  let b = Array.init total_length (fun _ -> 4.) in
  let b_vector = Mat.of_array b total_length 1 in
  let linear_system =
    Pvc.finit_differences_M2 (0., 0., 0., 0.) partitions ~width:1. ~height:1.
      b_vector
  in
  Mat.print linear_system

let t2 () =
  let partitions = 8 in
  let b = Array.init (partitions - 1) (fun _ -> 0.) in
  let b_vector = Mat.of_array b (partitions - 1) 1 in
  let linear_system =
    Pvc.finit_differences_M1 (0., 1.) ~width:1. partitions b_vector
  in
  Mat.print linear_system

let () =
  Printf.printf "Matriz 1 - Resultado\n";
  t1 ();
  Printf.printf "Matriz 2 - Resultado";
  t2 ()
