open Numerical

let write_data_to_file filename data_list =
  let oc = open_out filename in
  let () =
    List.iter
      (fun (time, position, speed) ->
        Printf.fprintf oc "%f,%f,%f\n" time position speed)
      data_list
  in
  close_out oc

let () =
  (* Velocidade inicial | Altura inicial *)
  let initial_state = Pvi.init_state 5. 200. in

  let pace = 0.00001 and maximum_time = 10. in

  let deriv (v0, _) _ = (-10. -. (0.25 *. v0 /. 2.), v0) in
  let data_list_RK = Pvi.get_data_RK initial_state 0. deriv pace maximum_time in
  let data_list_PC =
    Pvi.get_data_PC initial_state 0. deriv pace maximum_time epsilon_float
  in

  write_data_to_file "RK.csv" data_list_RK;
  write_data_to_file "PC.csv" data_list_PC
