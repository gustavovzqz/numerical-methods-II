open Owl

let get_inverse_dx_squared nodes =
  let square x = x *. x in
  square (float_of_int nodes)

(* TODO: Como saber se o elemento está na borda... não é tão simples... ou é? *)
let init_matrix bound size =
  Mat.init_2d size size (fun i j ->
      match bound i j with Some number -> number | None -> 0.)

let get_linear_system_M2 bound nodes =
  let matrix_size = (nodes - 1) * (nodes - 1) in
  (* TODO: Arrumar a função de inicialização (encontrar quem é a borda corretamente) e chamá-lá ao final do método *)
  let linear_system = init_matrix bound matrix_size
  and idx2 = get_inverse_dx_squared nodes in
  let idy2 = idx2 in

  let center_value = -2. *. (idx2 +. idy2)
  and side_value = idx2
  and vertical_value = idy2 in

  let validade_position pos = pos >= 0 && pos < matrix_size in

  for i = 0 to matrix_size - 1 do
    Printf.printf "Matriz da iteração %d\n" i;
    Mat.print linear_system;
    let rec set_pos_values val_list =
      match val_list with
      | (pos, value) :: t ->
          if validade_position pos then (
            Printf.printf "Setando na posição (%d, %d) o valor %f\n" i pos value;
            Mat.set linear_system i pos value);
          set_pos_values t
      | _ -> ()
    in

    let upper_pos = i + nodes - 1
    and lower_pos = i - nodes + 1
    and right_pos = i + 1
    and left_pos = i - 1
    and center_pos = i in

    let pos_val_list =
      [
        (upper_pos, vertical_value);
        (lower_pos, vertical_value);
        (right_pos, side_value);
        (left_pos, side_value);
        (center_pos, center_value);
      ]
    in

    set_pos_values pos_val_list
  done;

  linear_system
