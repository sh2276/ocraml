(** Module to represent a perceptron and its operations. *)
module type PerceptronType = sig

    type 'a t
    (** Representation type of [Perceptron] *)

    val create : int -> 'a list -> 'a t
    (** [create n lst] will create a [Perceptron] with [n] inputs, and a list of distinct classes *)
    
    val predict : Vector.t -> 'a t -> 'a
    (** [predict inputs perceptron] makes a prediction based on the inputs. 
        Requires: [inputs] has a dimension of nx1 and [perceptron] was created with size n. *)
    
    val update_weights : float -> 'a -> Vector.t -> 'a t -> 'a t
    (** [update_weights learning_rate expected actual inputs perceptron] updates the weights of the 
        perceptron based on the error. *)
end

module Perceptron: PerceptronType