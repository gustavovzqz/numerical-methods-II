open Numerical

let () =
  let initial_state = Pvi.init_state 3. 150. in
  let expr (v0, _) _ = -10. -. v0 in
  let deriv = Pvi.init_derivative expr in
  Pvi.plot_function initial_state 0. deriv 0.1 15.
