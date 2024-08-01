open Owl

let x = Mat.empty 5 5;;


let get_inverse_dx_squared nodes = 
  let square x = x *. x in 
  square (1. /. (float_of_int nodes))

 

let finite_difference_M2 fun_xy bound nodes = 

  (* TODO: Intialize matrix with bound values *)
  let linear_system = Mat.zeros nodes nodes and 
  idx2 = get_inverse_dx_squared nodes in 
  let idy2 = idx2 in 

  let center_value = -2. *. (idx2 +. idy2) and 
  side_value = idx2 and 
  vertical_value = idy2 in 

  (* i + 3 -> upper value *) 
  (* i - 3 -> lower value *) 
  (* i + 1 -> right value *) 
  (* i - 1 -> left value  *) 

  let apply_mask r c = 
  for i = 0 to nodes do
    rv = 
    Mat.set linear_system i (get_upper_bound
  done




