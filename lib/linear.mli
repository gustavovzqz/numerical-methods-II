(* A vector is a COLUMN vector by default*)
type vector = float array

(* Matrix is a (COLUMNS ARRAY ) array, so matrix.(0) = vector *)
type matrix = float array array

val zero_vector : int -> vector
val zero_matrix : int -> matrix
val add_vectors : vector -> vector -> vector
val scale_vector : float -> vector -> vector
val vector_norm : vector -> float
val normalize_vector : vector -> vector
val dot_product : vector -> vector -> float
val apply_matrix : matrix -> vector -> vector
