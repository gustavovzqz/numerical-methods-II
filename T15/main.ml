open Numerical
open Owl

let () =
  let partitions = 8 in
  let linear_system = Pvc.get_linear_system_M2 (0., 0., 0., 0.) partitions in
  Mat.print linear_system;
  let sqr_partitions = (partitions - 1) * (partitions - 1) in
  let b_array = Array.init sqr_partitions (fun _ -> 4.) in
  let b = Mat.of_array b_array sqr_partitions 1 in
  Mat.print (Linalg.D.linsolve linear_system b)
