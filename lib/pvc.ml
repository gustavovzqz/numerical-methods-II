open Owl

let get_inverse_dx_squared nodes =
  let square x = x *. x in
  square (float_of_int nodes)


let get_side_values part =

let idx2 = get_inverse_dx_squared part in 
 let center_value = -2. *. (idx2 +. idx2)
  and side_value = idx2
  and vertical_value = idx2 in
(center_value, side_value, vertical_value)


let init_matrix rows cols f =
  Array.init rows (fun i ->
    Array.init cols (fun j -> f i j))

type node = 
  | Bound of float 
  | Equation 

let get_matrix_value i j ~bounds = 
  let (lower, left, upper, right) = bounds in 
  match (i, j) with 
  | (_, 0) -> Bound lower
  | (_, 1) -> Bound upper 
  | (0, _) -> Bound left 
  | (1, _) -> Bound right 
  | _ -> Equation

let get_linear_system_M2 bound partitions =
  let nodes = partitions + 1 in 
  let system_size = (partitions * partitions) in 

  let domain = init_matrix nodes nodes (get_matrix_value ~bounds:bound) and
  linear_system = Mat.empty system_size system_size in 

  let (center_value, side_value, vertical_value) = get_side_values partitions in 

  let eq_pos = ref 0 in 
  (* Domain = nodes x nodes, and we want to iter in the innner matrix, starting in [1][1] and going to [nodes - 2] [nodes - 2] *)
  for i = 1 to nodes - 2  do
    for j = 1 to nodes - 2 do
      (* TODO: Make the linear system! Creating a match function to get the correct value *)
    done
  done

  


