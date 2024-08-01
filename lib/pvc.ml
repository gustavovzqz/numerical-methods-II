open Owl

let x = Mat.empty 5 5

let get_inverse_dx_squared nodes =
  let square x = x *. x in
  square (1. /. float_of_int nodes)

let init_matrix bound size =
  Mat.init_2d size size (fun i j ->
      match bound i j with Some number -> number | None -> 0.)

let get_linear_system_M2 bound nodes =
  let linear_system = init_matrix bound nodes
  and idx2 = get_inverse_dx_squared nodes in
  let idy2 = idx2 in

  let center_value = -2. *. (idx2 +. idy2)
  and side_value = idx2
  and vertical_value = idy2 in

  let validade_position pos = pos >= 0 || pos < nodes in

  for i = 0 to nodes do
    let rec set_pos_values = function
      | (pos, value) :: t ->
          if validade_position pos then (
            Mat.set linear_system i pos value;
            set_pos_values t)
      | _ -> ()
    in

    let upper_pos = i + nodes - 1
    and lower_pos = i - nodes - 1
    and right_pos = i + 1
    and left_pos = i - 1
    and center_pos = i in

    let pos_val_list =
      [
        (upper_pos, vertical_value);
        (lower_pos, vertical_value);
        (upper_pos, vertical_value);
        (right_pos, side_value);
        (left_pos, side_value);
        (center_pos, center_value);
      ]
    in

    set_pos_values pos_val_list
  done;

  linear_system
