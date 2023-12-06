type t = Matrix.t * Vector.t
(* AF: A perceptron [(m, b)] is represented by a tuple of a vector [m] and a
   float representing bias [b]. RI: abs(b) < 1 *)

let create num_inputs num_classes =
  let weights = Matrix.init (Array.make_matrix num_classes num_inputs 0.0) in
  let biases = Vector.init (Array.make num_classes 0.0) in
  (weights, biases)

let predict inputs (m, b) =
  let open Matrix in
  assert_m_v_dim m inputs;
  let unbiased = m @ inputs in
  let open Vector in
  unbiased + b |> argmax

let update_weights _ (* learning_rate *) expected inputs (m, b) =
  let actual = predict inputs (m, b) in
  let _ (* error *) = if expected = actual then 0.0 else 1.0 in
  (* let updated_weights = Matrix.(init (Array.map () ()) ) in let
     updated_weights = Matrix.(m + (learning_rate *. error *. inputs)) in let
     updated_bias = Vector.(b + (learning_rate *. error)) in (updated_weights,
     updated_bias) *)
  (m, b)
