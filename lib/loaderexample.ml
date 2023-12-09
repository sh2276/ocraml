open Ocraml.Loader
open Bimage.Expr
open Bimage

let _ =
  let transformations = [ invert () ] in
  let files =
    [ "./lib/mnist_png/testing/0/3.png"; "./lib/mnist_png/testing/0/10.png" ]
  in

  let _ = to_string files gray transformations in
  let _ = dir_to_matrix "./lib/mnist_png/training/0/" gray transformations in

  (* get a matrix, which is a vector of (vector representing each image) *)
  let vector_of_imagevectors = to_matrix files gray transformations in
  Ocraml.Matrix.print_mat vector_of_imagevectors
