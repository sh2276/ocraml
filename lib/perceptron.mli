(** Module to represent a perceptron and its operations. *)

type 'a t
(** Representation type of [Perceptron] *)

val create : int -> int -> (int -> 'a) -> 'a t
(** [create n c map] creates a new perceptron with [n] inputs, with [c] classes. 
    Requires: [map] is a map from the integers [0, c - 1] to the set of specified results. *)

val predict : Vector.t -> 'a t -> 'a
(** [predict inputs perceptron] makes a prediction based on the inputs. 
    Requires: [inputs] has a dimension of nx1 and [perceptron] was created with size n. *)

val update_weights : float -> 'a -> Vector.t -> 'a t -> 'a t
(** [update_weights learning_rate expected actual inputs perceptron] updates the weights of the 
    perceptron based on the error. *)