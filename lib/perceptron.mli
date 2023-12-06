(** Module to represent a perceptron and its operations. *)

type t
(** Representation type of [Perceptron] *)

(** [create size num_classes] creates a new perceptron with [size] inputs *)
val create : int -> int -> t

(** [predict inputs perceptron] makes a prediction based on the inputs. 
    Requires: [inputs] has a dimension of nx1 and [perceptron] was created with size n. *)
val predict : Vector.t -> t -> int

(** [update_weights learning_rate expected actual inputs perceptron] updates the weights of the perceptron based on the error. *)
val update_weights :
  float -> float -> Vector.t -> t -> t