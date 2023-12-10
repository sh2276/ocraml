(** Module to load image data for use in training and running perceptron model.*)

open Bimage
open Bimage.Expr

val file_to_img :
  ([< `Gray | `Rgb | `Rgba ] as 'a) Color.t ->
  string ->
  (float, f32, 'a) Image.t
(** [file_to_img] converts the string of a file path to the Image in the path *)

val files_to_imglist :
  ([< `Gray | `Rgb | `Rgba ] as 'a) Color.t ->
  string list ->
  (float, f32, 'a) Image.t list
(** [files_to_imglist] contains a list of Images that correspond to each of the
    files in the files argument *)

val transform_image :
  pixel t list -> ('a, 'b, 'c) Image.t -> ('a, 'b, 'c) Image.t
(** [transform_image] takes a list of transformations, from Bimage.Expr, and
    applies them to an Image [img]. This will help us augment data to make our
    model more robust to rotations and other transforms. Example usage:
    transform_image [[(invert ())]] img *)

val transform_all_images :
  pixel t list -> ('a, 'b, 'c) Image.t list -> ('a, 'b, 'c) Image.t list
(** [transform_all_images] applies [transform_image] to all images in the list
    passed in as the argument *)

val listify : ('a, 'b, 'c) Image.t -> 'a list
(** [listify] converts a (float, f32, Spec.ColorModule.t) Image.t Image
    to a float list representing the gray value of each pixel*)

val listify_n: ('a, 'b, 'c) Image.t list -> 'a list list
(** [listify_n] converts a list of Images to to a list of lists of
    values, where the inner list of values represents an image *)

val strings_of_list : float list -> string list
(** [string_of_list] converts a float list representation of a picture
    into a string list for printing *)

val strings_of_list_list : float list list -> string list list
(** [strings_of_list_list] applies [strings_of_list] to each
    element of a list of float list representations of images *)

(* val to_matrix :
  string list -> [< `Gray | `Rgb | `Rgba ] Color.t -> pixel t list -> Matrix.t *)
(** [to_matrix] returns a vector of vectors (a Matrix), where each vector
    represents a the vector representation of an image in each file from M.files *)

(* val to_string :
  string list ->
  [< `Gray | `Rgb | `Rgba ] Color.t ->
  Expr.pixel Expr.t list ->
  string *)
(* [to_string] converts everything *)

(* val dir_to_matrix :
  string ->
  [< `Gray | `Rgb | `Rgba ] Color.t ->
  Expr.pixel Expr.t list ->
  Matrix.t *)

val to_vector_list : string list ->
  [< `Gray | `Rgb | `Rgba ] Color.t ->
  Expr.pixel Expr.t list ->
  Vector.t list