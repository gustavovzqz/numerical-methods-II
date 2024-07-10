type state = float * float 
type state_derivative = state -> float -> state

val init_state : float -> float -> state 
val add_state : state -> state -> state
val scale_state : float -> state -> state
val init_derivative: (state -> float -> float) -> state_derivative 
val runge_kutta: state -> float -> state_derivative -> float -> state 
val get_nth_state: state -> float -> state_derivative -> float -> float -> state 
val plot_function: state -> float -> state_derivative -> float -> float -> unit
