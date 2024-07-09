type state = float * float 
type state_derivative = state -> float -> state 

let init_state f1 f2 = f1 * f2 

let derivative_of_state state_ expr t1 = 
  let (v0, _) = state_ in 
  (expr state_ t1, v0)

let init_derivative state_ = derivative_of_state state_ 

let runge_kutta state_ t_i derivative dt = 
  let f1 = derivative state_ t_i in 
  let s_one_half = add state_ (scale_state (dt /. 2.) f1) in 
  let f2 = derivative s_one_half (t_i +. (dt /. 2.)) in 
  let s_aprox = state_ 
  (* da pra seguir assim, depois tornar genérico *) 
