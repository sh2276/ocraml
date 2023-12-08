open Ocraml.Loader
open Bimage.Expr

module LSpec : LoaderSpec = struct
  let transformations = [ invert () ]

  let files =
    [ "./lib/mnist_png/testing/0/3.png"; "./lib/mnist_png/testing/0/10.png" ]
end

module L = Loader (LSpec)

let _ =
  let vallistlist =
    L.files_to_imglist |> L.transform_all_images |> L.imglist_to_vallistlist
    |> L.vallistlist_to_stringlistlist
  in

  let printcomma x = print_string (x ^ ", ") in

  match vallistlist with
  | [] -> [ print_string "nothing" ]
  | h :: _ -> List.map printcomma h

let _ =
  (* get a matrix, which is a vector of (vector representing each image) *)
  let vector_of_imagevectors = L.to_matrix in
  Ocraml.Matrix.print_mat vector_of_imagevectors
