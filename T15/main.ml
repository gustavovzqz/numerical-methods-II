open Numerical
open Owl

let t1 () =
  let partitions = 8 in
  let b = Array.init 49 (fun _ -> 4.) in
  let b_vector = Mat.of_array b 49 1 in
  let linear_system =
    Pvc.get_linear_system_M2 (0., 0., 0., 0.) partitions ~width:1. ~height:1.
      b_vector
  in
  Mat.print linear_system

(* let t2 () =
   let partitions = 8 in
   let b_vector = Mat.of_array [| 0.; 0.; 0.; 0.; 0.; 0.; 0. |] 7 1 in
   let linear_system =
     Pvc.get_linear_system_M1 (0., 1.) ~width:1. partitions b_vector
   in
   Mat.print linear_system *)

let () = t1 ()
