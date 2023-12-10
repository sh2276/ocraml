type t = float array array
(* A matrix is represented as an array of float arrays. *)

let init (m : float array array) = m
let num_rows (mat : t) = Array.length mat
let num_cols (mat : t) = Array.length mat.(0)

(* Checks if a matrix has a number of rows equal to the length of a row
   [Vector]. *)
let assert_m_v_dim (mat : t) (vec : Vector.t) =
  if num_cols mat <> Vector.length vec then
    invalid_arg "Matrix and vector are not compatible lengths"

(* Checks if two matrices have compatible dimensions for multiplication. *)
let assert_m_m_dim (m1 : t) (m2 : t) =
  if num_cols m1 <> num_rows m2 then
    invalid_arg "Matrices have incompatible dimensions for multiplication"

let assert_m_m_dim_exact (m1 : t) (m2 : t) =
  if num_cols m1 <> num_cols m2 || num_rows m1 <> num_rows m2 then
    invalid_arg "Matrices are not the same dimension."

(* Multiplies a matrix and a vector. *)
let mat_vec_prod (mat : t) (vec : Vector.t) =
  assert_m_v_dim mat vec;
  Vector.(init (Array.map (fun row -> Vector.init row @ vec) mat))

(* Multiplies two matrices. *)
let mat_mat_prod (m1 : t) (m2 : t) =
  assert_m_m_dim m1 m2;
  let nr1 = num_rows m1 in
  let nc1 = num_cols m1 in
  let nc2 = num_cols m2 in

  let prod = Array.make_matrix nr1 nc2 0.0 in

  for i = 0 to nr1 - 1 do
    for j = 0 to nc2 - 1 do
      let sum = ref 0.0 in
      for k = 0 to nc1 - 1 do
        sum := !sum +. (m1.(i).(k) *. m2.(k).(j))
      done;
      prod.(i).(j) <- !sum
    done
  done;
  prod

let mat_mat_add (m1 : t) (m2 : t) =
  assert_m_m_dim_exact m1 m2;
  let r = num_rows m1 in
  let c = num_cols m2 in
  let sum = Array.make_matrix r c 0.0 in
  for i = 0 to r - 1 do
    for j = 0 to c - 1 do
      sum.(i).(j) <- m1.(i).(j) +. m2.(i).(j)
    done
  done;
  sum

let ( @ ) m v = mat_vec_prod m v
let ( * ) m1 m2 = mat_mat_prod m1 m2
let ( + ) m1 m2 = mat_mat_add m1 m2

(* Transposes the matrix. *)
let transpose mat =
  let nr = num_rows mat in
  let nc = num_cols mat in
  let trans = Array.make_matrix nc nr 0.0 in
  for i = 0 to nr - 1 do
    for j = 0 to nc - 1 do
      trans.(j).(i) <- mat.(i).(j)
    done
  done;
  trans

(* Converts a matrix to a float array array. *)
let to_array (m : t) = m
