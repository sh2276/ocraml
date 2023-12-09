module Class = struct
  type 'a t = (int * 'a) list
  (** AF: Association list of keys and classes. RI: The map resulting from the
      association list must be one-to-one. The set of keys must be from
      [0, n-1]. *)

  (** [create lst] creates a new [Class] with the bindings present in the
      association list. Requires: [lst] must be a one-to-one mapping from each
      int to each result. The set of keys must be [0, ..., n-1] where n is the
      number of distinct results. *)
  let create lst = lst

  (** [get classes n] will return the binding with [n] as the key. *)
  let get lst n = List.assoc n lst

  (** [index classes r] will return the binding with [r] as the result. *)
  let index lst n = List.find (fun p -> snd p = n) lst |> fst

  (** [num_of_classes] will return the number of classes in a [Class]. *)
  let num_of_classes lst = List.length lst
end

module type PerceptronType = sig
  type 'a t

  val create : int -> (int * 'a) list -> 'a t
  val predict : Vector.t -> 'a t -> 'a
  val update_weights : float -> 'a -> Vector.t -> 'a t -> 'a t
end

module Perceptron : PerceptronType = struct
  type 'a return = 'a Class.t
  type 'a t = Matrix.t * Vector.t * 'a return
  (* AF: A perceptron [(m, b, f)] is represented by a tuple of a vector [m], a
     float representing bias [b], and binding function [f] that maps the
     integers [0, c - 1] to a set of results. RI: abs(b) < 1 *)

  let create num_inputs lst =
    let c = Class.create lst in
    let num_classes = Class.num_of_classes c in
    let weights = Matrix.init (Array.make_matrix num_classes num_inputs 0.0) in
    let biases = Vector.init (Array.make num_classes 0.0) in
    (weights, biases, c)

  let predict inputs (m, b, f) =
    let open Matrix in
    assert_m_v_dim m inputs;
    let unbiased = m @ inputs in
    let open Vector in
    unbiased + b |> argmax |> Class.get f

  let update_weights learning_rate expected inputs (m, b, f) =
    let actual = predict inputs (m, b, f) in
    if expected = actual then (m, b, f)
    else
      let target = Array.make (Vector.length b) 0.0 in
      target.(Class.index f actual) <- 1.0;
      target.(Class.index f expected) <- -1.0;
      let target_v = Vector.init target in

      let error = Vector.(target_v - b) in
      let add_w =
        Matrix.(
          ([| Vector.to_array error |] |> init |> transpose)
          * init [| Vector.(b |> ( * ) learning_rate |> to_array) |])
      in
      (Matrix.(m + add_w), Vector.(b + error), f)
end
