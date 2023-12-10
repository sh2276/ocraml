(** A [Class] is an util-module that generifies nessecary types and relevant
    helper functions. Given a list of classifications, it can map it between
    distinct integers and vice versa. *)
module Class = struct
  (** The representation type of a [Class]. *)
  type 'a t = 'a list
  (** AF: A map from index to class through a list. RI: List elements must be
      unique. *)

  (** [create lst] creates a new [Class] with the list of classes provided. *)
  let create lst = lst

  (** [get classes n] will return the [n]th class *)
  let get lst n = List.nth lst n

  (** [index classes r] will return the binding with [r] as the result. *)
  let index lst n =
    let rec index_aux lst acc =
      match lst with
      | [] -> raise (Failure "not found")
      | h :: t -> if n = h then acc else index_aux t (acc + 1)
    in
    index_aux lst 0

  (** [num_of_classes] will return the number of classes in a [Class]. *)
  let num_of_classes lst = List.length lst
end

type 'a t = Matrix.t * Vector.t * 'a Class.t
(* AF: A perceptron [(m, b, f)] is represented by a tuple of a vector [m], a
   float representing bias [b], and binding function [f] that maps the integers
   [0, c - 1] to a set of results. RI: abs(b) < 1 *)

let create num_inputs lst =
  let c = Class.create lst in
  let num_classes = Class.num_of_classes c in
  let weights = Matrix.init (Array.make_matrix num_inputs num_classes 0.0) in
  let biases = Vector.init (Array.make num_classes 0.0) in
  (weights, biases, c)

let predict inputs (m, b, f) =
  let open Matrix in
  let m = transpose m in
  assert_m_v_dim m inputs;
  let unbiased = m @ inputs in
  let open Vector in
  unbiased + b |> argmax |> Class.get f

let update_weights learning_rate inputs expected (m, b, f) =
  let actual = predict inputs (m, b, f) in
  if expected = actual then (m, b, f)
  else
    let target = Array.make (Vector.length b) 0.0 in
    target.(Class.index f actual) <- -1.0;
    target.(Class.index f expected) <- 1.0;
    let target_v = Vector.init target in

    let error = Vector.(target_v |> ( * ) learning_rate) in
    let add_w =
      Matrix.(
        ([| Vector.to_array error |] |> init |> transpose)
        * init [| Vector.(inputs |> to_array) |]
        |> transpose)
    in
    (Matrix.(m + add_w), Vector.(b + error), f)

let train_once rate lst perceptron =
  let rec train_aux lst acc =
    match lst with
    | [] -> acc
    | (v, e) :: t ->
        let newp = update_weights rate v e acc in
        train_aux t newp
  in
  train_aux lst perceptron

let train_epoch rate num lst perceptron =
  let rec train_aux num perceptron =
    match num with
    | 0 -> perceptron
    | _ -> train_aux Stdlib.(num - 1) (train_once rate lst perceptron)
  in
  train_aux num perceptron
