open Owl

let get_inverse_squared x = 1. /. (x *. x)

let get_side_values_M2 part width height =
  let part = float_of_int part in
  let idx = width /. part and idy = height /. part in

  let idx2 = get_inverse_squared idx and idy2 = get_inverse_squared idy in

  let center_value = -2. *. (idx2 +. idy2)
  and side_value = idx2
  and vertical_value = idy2 in
  (center_value, side_value, vertical_value)

let get_side_values_M1 part width =
  let part = float_of_int part in
  let idx = width /. part in

  let idx2 = get_inverse_squared idx in

  let center_value = (-2. *. idx2) -. 1. in
  let side_value = idx2 in
  (center_value, side_value)

let init_matrix rows cols f =
  Array.init rows (fun i -> Array.init cols (fun j -> f i j))

type node = Left | Right | Upper | Down | Equation

let get_matrix_value i j ~limit =
  match (i, j) with
  | _, 0 -> Left
  | lim, _ when lim = limit -> Upper
  | 0, _ -> Down
  | _, lim when lim = limit -> Right
  | _ -> Equation

let finit_differences_M1 (left_bound, right_bound) ~width partitions b_vector =
  let system_size = partitions - 1 in
  let center_value, side_value = get_side_values_M1 partitions width in

  let linear_system = Mat.empty system_size system_size in

  let write_linear_system i j value =
    if j >= 0 && j < system_size then Mat.set linear_system i j value else ()
  in

  for i = 0 to system_size - 1 do
    let left_position = i - 1 and right_position = i + 1 in

    let get_position_value pos value =
      let b_value = Mat.get b_vector i 0 in
      match pos with
      | -1 ->
          Mat.set b_vector i 0 (b_value -. (left_bound *. value));
          0.
      | k when k = system_size ->
          Mat.set b_vector i 0 (b_value -. (right_bound *. value));
          0.
      | _ -> value
    in

    let left_value = get_position_value left_position side_value
    and right_value = get_position_value right_position side_value in

    write_linear_system i left_position left_value;
    write_linear_system i i center_value;
    write_linear_system i right_position right_value
  done;
  Linalg.D.linsolve linear_system b_vector

let finit_differences_M2 (left_bound, up_bound, right_bound, down_bound) ~width
    ~height partitions b_vector =
  let equations_per_line = partitions - 1 in
  let system_size = equations_per_line * equations_per_line in

  Printf.printf
    "Partições: [%d]\nEquações por linha: [%d]\nTamanho do sistema: [%d]\n"
    partitions equations_per_line system_size;

  let domain =
    init_matrix (partitions + 1) (partitions + 1)
      (get_matrix_value ~limit:partitions)
  and linear_system = Mat.empty system_size system_size in

  let center_value, side_value, vertical_value =
    get_side_values_M2 partitions width height
  in

  let eq_pos = ref 0 in
  for i = 1 to partitions - 1 do
    for j = 1 to partitions - 1 do
      let get_matrix_value node value =
        let b_value = Mat.get b_vector !eq_pos 0 in
        match node with
        | Left ->
            Mat.set b_vector !eq_pos 0 (b_value -. (left_bound *. value));
            0.
        | Right ->
            Mat.set b_vector !eq_pos 0 (b_value -. (right_bound *. value));
            0.
        | Upper ->
            Mat.set b_vector !eq_pos 0 (b_value -. (up_bound *. value));
            0.
        | Down ->
            Mat.set b_vector !eq_pos 0 (b_value -. (down_bound *. value));
            0.
        | Equation -> value
      in

      let left_value = get_matrix_value domain.(i).(j - 1) vertical_value
      and right_value = get_matrix_value domain.(i).(j + 1) vertical_value
      and up_value = get_matrix_value domain.(i + 1).(j) side_value
      and down_value = get_matrix_value domain.(i - 1).(j) side_value in

      let write_linear_system position value =
        if position >= 0 && position < system_size then
          Mat.set linear_system !eq_pos position value
        else ()
      in

      write_linear_system (!eq_pos - 1) left_value;
      write_linear_system (!eq_pos + 1) right_value;
      write_linear_system (!eq_pos + equations_per_line) up_value;
      write_linear_system (!eq_pos - equations_per_line) down_value;
      write_linear_system !eq_pos center_value;

      eq_pos := !eq_pos + 1
    done
  done;
  Linalg.D.linsolve linear_system b_vector
