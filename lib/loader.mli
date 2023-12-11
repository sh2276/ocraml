(** Module to load image data for use in training and running perceptron model.*)

open Bimage
open Bimage.Expr

val file_to_img :
  ([< `Gray | `Rgb | `Rgba ] as 'a) Color.t ->
  string ->
  (float, f32, 'a) Image.t
(** [file_to_img colortype file] converts the string of a file path to a Bimage
    Image loaded in with the color type*)

val files_to_imglist :
  ([< `Gray | `Rgb | `Rgba ] as 'a) Color.t ->
  string list ->
  (float, f32, 'a) Image.t list
(** [files_to_imglist] contains a list of Images that correspond to each of the
    files in the files argument *)

val transform_image :
  pixel t list -> ('a, 'b, 'c) Image.t -> ('a, 'b, 'c) Image.t
(** [transform_image transforms img] takes a list of transformations, from
    Bimage.Expr, and applies them to an Image [img]. This will help us augment
    data to make our model more robust to rotations and other transforms.
    Example usage: transform_image [[Bimage.Expr.invert ()]] img *)

val transform_all_images :
  pixel t list -> ('a, 'b, 'c) Image.t list -> ('a, 'b, 'c) Image.t list
(** [transform_all_images] applies [transform_image] to all images in the list
    passed in as the argument *)

val listify : ('a, 'b, 'c) Image.t -> 'a list
(** [listify img] converts a Bimage Image to a float list representing each
    pixel*)

val listify_n : ('a, 'b, 'c) Image.t list -> 'a list list
(** [listify_n] converts a list of Images to to a list of lists of values, where
    the inner list of values represents an image *)

val to_vector_list :
  string list ->
  [< `Gray | `Rgb | `Rgba ] Color.t ->
  Expr.pixel Expr.t list ->
  Vector.t list
(** [to_vector_list files colortype transformations] takes a list of files, a
    Bimage color type and a list of transformations to output a list of
    [Vector], each of which represents an image loaded with a colortype and
    having the transformations applied to it *)

val to_string :
  string list ->
  [< `Gray | `Rgb | `Rgba ] Color.t ->
  Expr.pixel Expr.t list ->
  string
(* [to_string imagepaths colortype transformations] takes in a list of file
   paths and outputs a string that shows the pixel values of all images. *)

val if_names : string -> string list
(** [if_names dir] takes in a string corresponding to a directory and outputs a
    list of files in that directory *)

val shuffle_list : 'a list -> 'a list
(** [shuffle_list lst] shuffles a list using the Fisher-Yates shuffling
    algorithm in order to randomize the order of training inputs*)

val label_files : (string * string) list -> (string * string) list
(* [label_files folders] labels files in a directory with the right class
   embedding.*)
