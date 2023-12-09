module type ClassType = sig
  type 'a t

  val create : (int * 'a) list -> 'a t
  val get : 'a t -> int -> 'a
  val index : 'a t -> 'a -> int
  val num_of_classes : 'a t -> int
end

module Class : ClassType = struct
  type 'a t = (int * 'a) list
  (** AF: Association list of keys and classes. RI: The map resulting from the
      association list must be one-to-one. The set of keys must be from
      [0, n-1]. *)

  let create lst = lst
  let get lst n = List.assoc n lst
  let index lst n = List.find (fun p -> snd p = n) lst |> fst
  let num_of_classes lst = List.length lst
end

module type Perceptron = sig
  module C : ClassType

  type 'a return = 'a C.t
  type 'a t

  val create : int -> (int * 'a) list -> 'a t
  val predict : Vector.t -> 'a t -> 'a
  val update_weights : float -> 'a -> Vector.t -> 'a t -> 'a t
end

module PerceptronBuilder (C : ClassType) : Perceptron with module C = C = struct
  module C = C

  type 'a return = 'a C.t
  type 'a t = Matrix.t * Vector.t * 'a return
  (* AF: A perceptron [(m, b, f)] is represented by a tuple of a vector [m], a
     float representing bias [b], and binding function [f] that maps the
     integers [0, c - 1] to a set of results. RI: abs(b) < 1 *)

  let create num_inputs lst =
    let c = C.create lst in
    let num_classes = C.num_of_classes c in
    let weights = Matrix.init (Array.make_matrix num_classes num_inputs 0.0) in
    let biases = Vector.init (Array.make num_classes 0.0) in
    (weights, biases, c)

  let predict inputs (m, b, f) =
    let open Matrix in
    assert_m_v_dim m inputs;
    let unbiased = m @ inputs in
    let open Vector in
    unbiased + b |> argmax |> C.get f

  let update_weights learning_rate expected inputs (m, b, f) =
    let actual = predict inputs (m, b, f) in
    if expected = actual then (m, b, f)
    else
      let target = Array.make (Vector.length b) 0.0 in
      target.(C.index f actual) <- 1.0;
      target.(C.index f expected) <- -1.0;
      let target_v = Vector.init target in

      let error = Vector.(target_v - b) in
      let add_w =
        Matrix.(
          ([| Vector.to_array error |] |> init |> transpose)
          * init [| Vector.(b |> ( * ) learning_rate |> to_array) |])
      in
      (Matrix.(m + add_w), Vector.(b + error), f)
end
