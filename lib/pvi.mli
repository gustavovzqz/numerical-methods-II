type state = float * float 
type state_derivative = state -> float -> state

val init_state : float -> float -> state 
val add_state : state -> state -> state
val scale_state : float -> state -> state
val third_runge_kutta: state -> float -> state_derivative -> float -> state 
val fourth_runge_kutta: state -> float -> state_derivative -> float -> state
val predictor_corrector: state -> state -> state -> state -> float -> state_derivative -> float ->float -> state 
val get_data_RK: state -> float -> state_derivative -> float -> float -> (float * float * float) list
 val get_data_PC: state -> float -> state_derivative -> float -> float -> float -> (float * float * float) list 
