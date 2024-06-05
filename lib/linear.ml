type vector = float array
type matrix = float array array

let zero_vector n = Array.make n 0.

(* TODO: Will it always be a square matrix? *)
let zero_matrix n = Array.init n (fun _ -> zero_vector n)

let add_vectors v1 v2 =
  let n1 = Array.length v1 and n2 = Array.length v2 in
  assert (n1 = n2);
  Array.init n1 (fun i -> v1.(i) +. v2.(i))

let scale_vector k vec =
  let n = Array.length vec in
  Array.init n (fun i -> k *. vec.(i))

let vector_norm vec =
  let sum_of_squares = Array.fold_left (fun acc x -> acc +. (x *. x)) 0. vec in
  sqrt sum_of_squares

let normalize_vector vec = scale_vector (vector_norm vec) vec

let dot_product v1 v2 =
  let n1 = Array.length v1 and n2 = Array.length v2 in
  assert (n1 = n2);

  let rec dot_prod (iter : int) (acc : float) : float =
    if iter = n1 then acc
    else dot_prod (iter + 1) (acc +. (v1.(iter) *. v2.(iter)))
  in

  dot_prod 0 0.

let apply_matrix mat vec =
  let columns = Array.length mat and n = Array.length vec in
  assert (n = columns);
  Array.init n (fun i -> dot_product mat.(i) vec)
