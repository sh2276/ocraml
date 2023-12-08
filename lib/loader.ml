open Bimage
open Bimage_unix
open Bimage.Expr

module type LoaderSpec = sig
  val transformations : pixel t list
  val files : string list
end

module Loader (M : LoaderSpec) = struct
  let file_to_img img =
    match Magick.read f32 gray img with
    | Ok img -> img
    | Error e -> failwith (Error.to_string e)

  let files_to_imglist = List.map file_to_img M.files

  let rec transform_image remaining_transforms
      (img : (float, f32, [ `Gray ]) Image.t) =
    match remaining_transforms with
    | [] -> img
    | h :: t ->
        let _ = Filter.v h ~output:img [| Image.any img |] in
        transform_image t img

  let transform_all_images (img_list : (float, f32, [ `Gray ]) Image.t list) =
    List.map (transform_image M.transformations) img_list

  let img_to_vallist (img : (float, f32, [ `Gray ]) Image.t) =
    Array.to_list (Data.to_array (Image.data img))

  let imglist_to_vallistlist (img_list : (float, f32, [ `Gray ]) Image.t list) =
    List.map img_to_vallist img_list

  let vallist_to_stringlist (val_list : float list) =
    let rec helper val_list acc =
      match val_list with
      | [] -> acc
      | h :: t -> helper t (string_of_float h :: acc)
    in
    List.rev (helper val_list [])

  let vallistlist_to_stringlistlist (val_listlist : 'a list list) =
    List.map vallist_to_stringlist val_listlist

  let to_matrix =
    let vallistlist =
      files_to_imglist |> transform_all_images |> imglist_to_vallistlist
    in
    let valarrarr = Array.of_list (List.map Array.of_list vallistlist) in
    Matrix.init valarrarr
end
