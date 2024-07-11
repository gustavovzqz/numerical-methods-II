open Numerical

let () =
  (* Velocidade inicial | Altura inicial *)
  let initial_state = Pvi.init_state 5. 200. in

  (* Expressão final que define a derivada *)
  let expr (v0, _) _ = -10. -. (0.25 /. 2. *. v0) in
  let deriv = Pvi.init_derivative expr in
  Pvi.plot_function_fst initial_state 0. deriv 0.00001 15.;
  Pvi.plot_function_snd initial_state 0. deriv 0.00001 15.
