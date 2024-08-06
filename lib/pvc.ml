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
  Array.init rows (fun i -> Array.init cols (fun j -> f i j))

type node = Bound of float | Equation

let get_matrix_value i j ~bounds ~limit =
  let lower, left, upper, right = bounds in
  match (i, j) with
  | 0, _ -> Bound lower
  | lim, _ when lim = limit -> Bound upper
  | _, 0 -> Bound left
  | _, lim when lim = limit -> Bound right
  | _ -> Equation

let get_linear_system_M2 bound partitions =
  let equations_per_line = partitions - 1 in
  let system_size = equations_per_line * equations_per_line in

  Printf.printf
    "Partições: [%d]\nEquações por linha: [%d]\nTamanho do sistema: [%d]\n"
    partitions equations_per_line system_size;

  let domain =
    init_matrix (partitions + 1) (partitions + 1)
      (get_matrix_value ~bounds:bound ~limit:partitions)
  and linear_system = Mat.empty system_size system_size in

  let center_value, side_value, vertical_value = get_side_values partitions in

  let get_side_value = function Bound num -> num | Equation -> side_value
  and get_vertical_value = function
    | Bound num -> num
    | Equation -> vertical_value
  in

  let eq_pos = ref 0 in
  (* Domain = nodes x nodes, and we want to iter in the innner matrix, starting in [1][1] and going to [nodes - 2] [nodes - 2] *)
  for i = 1 to partitions - 1 do
    for j = 1 to partitions - 1 do
      (*       Printf.printf "Estou na posição [%d][%d]\n" i j; *)
      let left_value = get_side_value domain.(i).(j - 1)
      and right_value = get_side_value domain.(i).(j + 1)
      and up_value = get_vertical_value domain.(i + 1).(j)
      and down_value = get_vertical_value domain.(i - 1).(j) in

      (*       Printf.printf "Esquerda: [%f]\nDireita: [%f]\nCima: [%f]\nBaixo: [%f]" *)
      (*         left_value right_value up_value down_value; *)

      (* Writing the values *)
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
  linear_system
