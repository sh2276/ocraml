(** Module to represent a perceptron and its operations. *)

type 'a t
(** Representation type of [Perceptron] *)

val create : int -> 'a list -> 'a t
(** [create n lst] will create a [Perceptron] with [n] inputs, and a list of
    distinct classes.

    Given a variant type [A | B | C], a 4-input perceptron can be created by
    calling [create 4 [A; B; C]]. *)

val predict : Vector.t -> 'a t -> 'a
(** [predict inputs perceptron] makes a prediction based on the inputs.
    Requires: [inputs] has a dimension of nx1 and [perceptron] was created with
    size n. *)

val update_weights : float -> Vector.t -> 'a -> 'a t -> 'a t
(** [update_weights rate expected inputs perceptron] updates the weights of the
    perceptron based on the given vector. [rate] is the specified learning rate
    of the perceptron. *)

val error_rate : (Vector.t * 'a) list -> 'a t -> float
(** [error_rate lst perceptron] checks every input against the perceptron and
    calculates what percentage of it is accurately guessed.*)

val train :
  float -> float -> int -> (Vector.t * 'a) list -> 'a t -> 'a t * float list
(** [train rate margin num_of_epochs lst perceptron] will train the perceptron
    either [num_of_epoch] times on [lst], which is an association list of
    vectors and their corresponding expected outputs, or until the margin of
    error is below [margin]. Whichever is achieved first will return the
    perceptron. A list of margin of errors is also returned in order of
    ascending epochs. [rate] is the specified learning rate of the perceptron. *)
