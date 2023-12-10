(** Module to load image data for use in training and running perceptron model.*)

open Bimage
open Bimage.Expr
(* (** A [LoaderSpec] specifies which files will be loaded in and the
   transformations that will be applied to them. ` *) module type LoaderSpec =
   sig module ColorModule : COLOR

   type color = [ `Gray | `Rgb | `Rgba ] Color.t

   val transformations : pixel t list (** [transformations] is a list of
   transformations from Bimage.Expr*)

   val files : string list (** [files] is a list of files containing images we
   want to use *) end *)

(* module Loader : sig *)
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

val img_to_vallist : ('a, 'b, 'c) Image.t -> 'a list
(** [img_to_vallist] converts a (float, f32, Spec.ColorModule.t) Image.t Image
    to a float list representing the gray value of each pixel*)

val imglist_to_vallistlist : ('a, 'b, 'c) Image.t list -> 'a list list

val vallist_to_stringlist : float list -> string list
(** [vallist_to_stringlist] converts a float list representation of a picture
    into a string list for printing *)

val vallistlist_to_stringlistlist : float list list -> string list list
(** [vallistlist_to_stringlistlist] applies [vallist_to_stringlist] to each
    element of a list of float list representations of images *)

val to_matrix :
  string list -> [< `Gray | `Rgb | `Rgba ] Color.t -> pixel t list -> Matrix.t
(** [to_matrix] returns a vector of vectors (a Matrix), where each vector
    represents a the vector representation of an image in each file from M.files *)
(* end *)

val to_string :
  string list ->
  [< `Gray | `Rgb | `Rgba ] Color.t ->
  Expr.pixel Expr.t list ->
  string

val dir_to_matrix :
  string ->
  [< `Gray | `Rgb | `Rgba ] Color.t ->
  Expr.pixel Expr.t list ->
  Matrix.t
