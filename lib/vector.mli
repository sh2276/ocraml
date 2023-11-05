(** Module to represent vectors and their related operations. *)

type t
(** Representation type of [Vector]. *)

val assert_eq_len : t -> t -> unit
(** Checks if the length of two vectors are the same. *)

val init : float array -> t
(** Create a vector from an array of floats *)

val add : t -> t -> t
(** Add two vectors element-wise *)

val ( + ) : t -> t -> t
(** Add two vectors element-wise (infix) *)

val sub : t -> t -> t
(** Subtract two vectors element-wise *)

val ( - ) : t -> t -> t
(** Subtract two vectors element-wise (infix) *)

val scalar_mult : float -> t -> t
(** Multiply a vector by a scalar *)

val ( * ) : float -> t -> t
(** Multiply a vector by a scalar (infix) *)

val dot_prod : t -> t -> float
(** Compute the dot product of two vectors *)

val ( @ ) : t -> t -> float
(** Compute the dot product of two vectors (infix) *)

val length : t -> int
(** The length of a vector *)

val to_array : t -> float array
(** Convert a vector to an array of floats. *)
