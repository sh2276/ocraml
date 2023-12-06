(** Module to represent a perceptron and its operations. *)

type t
(** Representation type of [Perceptron] *)

val create : int -> int -> t
(** [create size num_classes] creates a new perceptron with [size] inputs *)

val predict : Vector.t -> t -> int
(** [predict inputs perceptron] makes a prediction based on the inputs. 
    Requires: [inputs] has a dimension of nx1 and [perceptron] was created with size n. *)

val update_weights : int -> int -> Vector.t -> t -> t
(** [update_weights learning_rate expected actual inputs perceptron] updates the weights of the perceptron based on the error. *)