type state = float * float 
type state_derivative = state -> float -> state

val init_state : float -> float -> state 
val add_state : state -> state -> state
val scale_state : float -> state -> state
val third_runge_kutta: state -> float -> state_derivative -> float -> state 
val fourth_runge_kutta: state -> float -> state_derivative -> float -> state
val predictor_corrector: state -> state -> state -> state -> float -> state_derivative -> float ->float -> state 
val plot_function_fst: state -> float -> state_derivative -> float -> float -> unit
val plot_function_snd: state -> float -> state_derivative -> float -> float -> unit
 val plot_function_fst_PC: state -> float -> state_derivative -> float -> float -> float -> unit
 val plot_function_snd_PC: state -> float -> state_derivative -> float -> float -> float -> unit
