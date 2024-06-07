type vector = float array
type matrix = float array array

val add_vectors : vector -> vector -> vector
val identity_matrix : int -> matrix
val scale_vector : float -> vector -> vector
val vector_norm : vector -> float
val normalize_vector : vector -> vector
val dot_product : vector -> vector -> float
val apply_matrix : matrix -> vector -> vector
val regular_power_iteration : matrix -> vector -> float -> float * vector
val print_vector : vector -> unit
val lu_decomposition : matrix -> matrix * matrix
val solve_lu : matrix -> vector -> vector