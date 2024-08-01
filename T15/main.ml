open Numerical
open Owl

let border_function x y =
  match (x, y) with
  | _, 0 -> Some 0.
  | _, 1 -> Some 0.
  | 0, _ -> Some 0.
  | 1, _ -> Some 0.
  | _ -> None

let () = Mat.print (Pvc.get_linear_system_M2 border_function 4)
