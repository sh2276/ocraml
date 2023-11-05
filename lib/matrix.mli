(** Module to represent matricies and their related operations. *)

type t
(** Representation type of [Matrix].*)

val mat_vec_dim_ok : t -> Vector.t -> unit
(** Checks if a vector has the same length as the number of rows in a matrix.*)

val mat_mat_dim_ok : t -> t -> unit
(** Checks if a two matricies have the same dimensions.*)

val init : float array array -> t
(** Initializes a [Matrix] from a 2D array of floats. *)

val mat_vec_prod : t -> Vector.t -> Vector.t
(** Matrix-vector product of [Matrix] mat and [Vector] vec. Requires: [Matrix]
    and [Vector] have compatible lengths (i.e. n*m matrix and an n*1 vector). *)

val transpose : t -> t
(** Transpose of [Matrix]*)

val num_rows : t -> int
(** Number of rows of a [Matrix]*)

val num_cols : t -> int
(** Number of columns of a [Matrix]*)

val to_array : t -> float array array
(** Convert a matrix to an array of float arrays. *)
