(** Module to represent a perceptron and its operations. *)

module type ClassType = sig
    type 'a t
    (** Representation type of a [Class] *)
  
    val create : (int * 'a) list -> 'a t
    (** [create lst] creates a new [Class] with the bindings present in the
        association list. Requires: [lst] must be a one-to-one mapping from each
        int to each result. The set of keys must be [0, ..., n-1] where n is the
        number of distinct results. *)
  
    val get : 'a t -> int -> 'a
    (** [get classes n] will return the binding with [n] as the key. *)
  
    val index : 'a t -> 'a -> int
    (** [index classes r] will return the binding with [r] as the result. *)

    val num_of_classes : 'a t -> int
    (** [num_of_classes] will return the number of classes in a [Class]. *)
  end

module Class : ClassType

module type Perceptron = sig
    module C : ClassType

    type 'a return = 'a C.t
    (** [return] is the type of the class. *)

    type 'a t
    (** Representation type of [Perceptron] *)

    val create : int -> (int * 'a) list -> 'a t
    (** *)
    
    val predict : Vector.t -> 'a t -> 'a
    (** [predict inputs perceptron] makes a prediction based on the inputs. 
        Requires: [inputs] has a dimension of nx1 and [perceptron] was created with size n. *)
    
    val update_weights : float -> 'a -> Vector.t -> 'a t -> 'a t
    (** [update_weights learning_rate expected actual inputs perceptron] updates the weights of the 
        perceptron based on the error. *)
end

module PerceptronBuilder (C : ClassType) : Perceptron with module C = C