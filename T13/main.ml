open Numerical

let () =
  (* Velocidade inicial | Altura inicial *)
  let initial_state = Pvi.init_state 5. 200. in

  (* Definindo a derivada *)
  let deriv (v0, _) _ = (-10. -. (0.25 *. v0 /. 2.), v0) in
  Pvi.plot_function_fst initial_state 0. deriv 0.1 15.;
  Pvi.plot_function_snd initial_state 0. deriv 0.1 15.
