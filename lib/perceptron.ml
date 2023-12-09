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

let update_weights learning_rate expected inputs (m, b) =
  let actual = predict inputs (m, b) in
  if expected = actual then (m, b)
  else
    let target = Array.make (Vector.length b) 0.0 in
    target.(actual) <- 1.0;
    target.(expected) <- -1.0;
    let target_v = Vector.init target in

    let error = Vector.(target_v - b) in
    let add_w =
      Matrix.(
        ([| Vector.to_array error |] |> init |> transpose)
        * init [| Vector.(b |> ( * ) learning_rate |> to_array) |])
    in
    (Matrix.(m + add_w), Vector.(b + error))
