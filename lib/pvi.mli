type state = float * float 
type state_derivative = state -> float -> state

val init_state : float -> float -> state 
val add_state : state -> state -> state
val scale_state : float -> state -> state
val init_derivative: (state -> float -> float) -> state_derivative 
val third_runge_kutta: state -> float -> state_derivative -> float -> state 
val fourth_runge_kutta: state -> float -> state_derivative -> float -> state 
val plot_function_fst: state -> float -> state_derivative -> float -> float -> unit
val plot_function_snd: state -> float -> state_derivative -> float -> float -> unit
