type t = float array array
(* A matrix is represented as an array of float arrays. *)

let init (m : float array array) = m
let num_rows (mat : t) = Array.length mat
let num_cols (mat : t) = Array.length mat.(0)

(* Checks if a matrix has a number of rows equal to the length of a row
   [Vector]. *)
let mat_vec_dim_ok (mat : t) (vec : Vector.t) =
  if num_rows mat <> Vector.length vec then
    invalid_arg "Matrix and vector are not compatible lengths"

(* Checks if a two matricies have the same dimensions. *)
let mat_mat_dim_ok (m1 : t) (m2 : t) =
  if num_rows m1 <> num_rows m2 || num_cols m1 <> num_cols m2 then
    invalid_arg "Matricies have incompatible dimensions"

(* Multiplies a matrix and a vector. *)
let mat_vec_prod (mat : t) (vec : Vector.t) =
  mat_vec_dim_ok mat vec;
  Vector.(init (Array.map (fun row -> Vector.init row @ vec) mat))

(* Transposes the matrix. *)
let transpose mat =
  let nr = num_rows mat in
  let nc = num_cols mat in
  let trans = Array.make_matrix nr nc 0.0 in
  for i = 0 to nr - 1 do
    for j = 0 to nc - 1 do
      trans.(j).(i) <- mat.(i).(j)
    done
  done;
  trans

(* Converts a matrix to a float array array. *)
let to_array (m : t) = m
