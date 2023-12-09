open Bimage
open Bimage_unix

let file_to_img colorType file =
  match Magick.read f32 colorType file with
  | Ok img -> img
  | Error e -> failwith (Error.to_string e)

let files_to_imglist colorType files = List.map (file_to_img colorType) files

let rec transform_image remaining_transforms img =
  match remaining_transforms with
  | [] -> img
  | h :: t ->
      let _ = Filter.v h ~output:img [| Image.any img |] in
      transform_image t img

let transform_all_images transformations img_list =
  List.map (transform_image transformations) img_list

let img_to_vallist img = Array.to_list (Data.to_array (Image.data img))
let imglist_to_vallistlist img_list = List.map img_to_vallist img_list

let vallist_to_stringlist (val_list : float list) =
  let rec helper val_list acc =
    match val_list with
    | [] -> acc
    | h :: t -> helper t (string_of_float h :: acc)
  in
  List.rev (helper val_list [])

let vallistlist_to_stringlistlist (val_listlist : 'a list list) =
  List.map vallist_to_stringlist val_listlist

let to_matrix files colortype transformations =
  let vallistlist =
    files_to_imglist colortype files
    |> transform_all_images transformations
    |> imglist_to_vallistlist
  in
  let valarrarr = Array.of_list (List.map Array.of_list vallistlist) in
  Matrix.init valarrarr

let to_string files colortype transformations =
  Matrix.to_string (to_matrix files colortype transformations)
