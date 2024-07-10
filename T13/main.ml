open Numerical

let () =
  let initial_state = Pvi.init_state 3. 150. in
  let expr (v0, _) _ = -10. -. v0 in
  let deriv = Pvi.init_derivative expr in
  let v1, y1 = Pvi.get_nth_state initial_state 0. deriv 0.1 1. in
  Printf.printf "Valor de v1: %f\nValor de y1:%f\n" v1 y1
