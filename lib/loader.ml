open Bimage
open Bimage_unix

(* Loads the image from the given path with a certain color type to a Bimage
   Image*)
let file_to_img colorType file =
  match Magick.read f32 colorType file with
  | Ok img -> img
  | Error e -> failwith (Error.to_string e)

(* Applies file_to_img to all file paths in a given list *)
let files_to_imglist colorType files = List.map (file_to_img colorType) files

(* Applies transformations in a given transformation list to the image *)
let rec transform_image remaining_transforms img =
  match remaining_transforms with
  | [] -> img
  | h :: t ->
      let _ = Filter.v h ~output:img [| Image.any img |] in
      transform_image t img

(* Applies transform_image to a list of images *)
let transform_all_images transformations img_list =
  List.map (transform_image transformations) img_list

(* Transforms a Bimage Image to a list of numbers representing each pixel *)
let listify img = Array.to_list (Data.to_array (Image.data img))

(* Applies listify to a list of images *)
let listify_n img_list = List.map listify img_list

(* Converts a list of files, given a color type and list of transformations, to
   a list or Vectors, each representing an image. *)
let to_vector_list files colortype transformations =
  let vallistlist =
    files_to_imglist colortype files
    |> transform_all_images transformations
    |> listify_n
  in
  List.map (fun x -> Vector.init (Array.of_list x)) vallistlist
