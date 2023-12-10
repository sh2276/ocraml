(** Module to represent vectors and their related operations. This module is
    designed to be a general-purpose vector module that replicates the common
    features of scientific computing libraries. This module is designed to
    provide a comprehensive repository of linear algebra functions, similar to
    Python's numpy, for use in data pipelines and developing machine learning
    models. *)

type t
(** Representation type of [Vector]. *)

val assert_eq_len : t -> t -> unit
(** Checks if the length of two vectors is the same. *)

val init : float array -> t
(** Initializes a [Vector] from an array of floats. *)

val add : t -> t -> t
(** Element-wise addition of two vectors. *)

val ( + ) : t -> t -> t
(** Element-wise addition of two vectors (infix). *)

val sub : t -> t -> t
(** Element-wise subtraction of two vectors. *)

val ( - ) : t -> t -> t
(** Element-wise subtraction of two vectors (infix). *)

val scalar_mult : float -> t -> t
(** Scalar multiplication of a [Vector] by a scalar. *)

val ( * ) : float -> t -> t
(** Scalar multiplication of a [Vector] by a scalar (infix). *)

val dot_prod : t -> t -> float
(** Compute the dot product of two [Vector]s. *)

val ( @ ) : t -> t -> float
(** Compute the dot product of two [Vector]s (infix). *)

val length : t -> int
(** The length of a [Vector]. *)

val to_array : t -> float array
(** Convert a [Vector] to an array of floats. *)

val argmax : t -> int
(** Finds the argmax of a [Vector]. If there are multiple maximums, [argmax v]
    will be the lowest index *)

val to_string : t -> string
(**Convert a [Vector] into a string for debug and printing purposes.*)

val print_vec : t -> unit
(**Print a [Vector] for debug and testing purposes.*)
