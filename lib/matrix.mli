(** Module to represent matricies and their related operations. *)

type t
(** Representation type of [Matrix].*)

val assert_m_v_dim : t -> Vector.t -> unit
(** Checks if a vector has the same length as the number of rows in a matrix.*)

val assert_m_m_dim : t -> t -> unit
(** Checks if a two matricies have the same dimensions.*)

val init : float array array -> t
(** Initializes a [Matrix] from a 2D array of floats. *)

val mat_vec_prod : t -> Vector.t -> Vector.t
(** Matrix-vector product of [Matrix] mat and [Vector] vec. Requires: [Matrix]
    and [Vector] have compatible dimensions (i.e. n*m matrix and an n*1 vector). *)

val ( @ ) : t -> Vector.t -> Vector.t
(** Matrix-vector product of [Matrix] mat and [Vector] vec. Requires: [Matrix]
    and [Vector] have compatible dimensions (i.e. n*m matrix and an n*1 vector).
    (Infix)*)


val mat_mat_prod : t -> t -> t
(** Matrix-matrix product of [Matrix] m1 and [Matrix] m2. Requires: [Matrix] m1
    and [Matrix] m2 have compatible dimensions (i.e. n*m matrix m1 and an m*r
    matrix m2). *)


val ( * ) : t -> t -> t
(** Matrix-matrix product of [Matrix] m1 and [Matrix] m2. Requires: [Matrix] m1
    and [Matrix] m2 have compatible dimensions (i.e. n*m matrix m1 and an m*r
    matrix m2). (Infix)*)

val transpose : t -> t
(** Transpose of [Matrix]*)

val num_rows : t -> int
(** Number of rows of a [Matrix]*)

val num_cols : t -> int
(** Number of columns of a [Matrix]*)

val to_array : t -> float array array
(** Convert a [Matrix] to an array of float arrays. *)
