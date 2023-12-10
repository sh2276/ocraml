(** Module to represent a perceptron and its operations. *)

type 'a t
(** Representation type of [Perceptron] *)

val create : int -> 'a list -> 'a t
(** [create n lst] will create a [Perceptron] with [n] inputs, and a list of
    distinct classes *)

val predict : Vector.t -> 'a t -> 'a
(** [predict inputs perceptron] makes a prediction based on the inputs.
    Requires: [inputs] has a dimension of nx1 and [perceptron] was created
    with size n. *)

val update_weights : float -> Vector.t -> 'a -> 'a t -> 'a t
(** [update_weights rate expected inputs perceptron] updates the
    weights of the perceptron based on the given vector. [rate] 
    is the specified learning rate of the perceptron. *)

val train_epoch : float -> int -> (Vector.t * 'a) list -> 'a t -> 'a t
(** [train_epoch learning_rate num_of_epochs lst perceptron] will train the perceptron 
    [num_of_epoch] times on [lst], which is an association list of vectors and their corresponding 
    expected outputs.. [rate] is the specified learning rate of the perceptron. *)
