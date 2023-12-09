open Ocraml.Loader
open Bimage.Expr
open Bimage

let _ =
  let transformations = [ invert () ] in
  let files =
    [ "./lib/mnist_png/testing/0/3.png"; "./lib/mnist_png/testing/0/10.png" ]
  in

  let vallistlist =
    files_to_imglist gray files
    |> transform_all_images transformations
    |> imglist_to_vallistlist |> vallistlist_to_stringlistlist
  in

  let printcomma x = print_string (x ^ ", ") in

  let _ =
    match vallistlist with
    | [] -> [ print_string "nothing" ]
    | h :: _ -> List.map printcomma h
  in

  (* get a matrix, which is a vector of (vector representing each image) *)
  let vector_of_imagevectors = to_matrix files gray transformations in
  Ocraml.Matrix.print_mat vector_of_imagevectors
