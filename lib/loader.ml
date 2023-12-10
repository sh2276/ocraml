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

let vallist_to_stringlist (val_list : float list) =
  let rec helper val_list acc =
    match val_list with
    | [] -> acc
    | h :: t -> helper t (string_of_float h :: acc)
  in
  List.rev (helper val_list [])

let vallistlist_to_stringlistlist (val_listlist : 'a list list) =
  List.map vallist_to_stringlist val_listlist

let to_string files colortype transformations =
  let vlist = to_vector_list files colortype transformations in
  let out = "" in
  let helper vlist acc =
    match vlist with
    | [] -> acc
    | h :: t -> acc ^ ", " ^ Vector.to_string h
  in
  helper vlist out

(* gets a list of files in the directory [dir] *)
let if_names in_dir =
  List.map (fun x -> in_dir ^ x) (Array.to_list (Sys.readdir in_dir))

(* Shuffle the list *)
(* Used Fisher-Yates shuffling algorithm to randomize the order of training
   inputs *)
let shuffle_list lst =
  let fisher_yates_shuffle arr =
    let len = Array.length arr in
    for i = 0 to len - 2 do
      let j = Random.int (len - i) + i in
      let temp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- temp
    done
  in
  let arr = Array.of_list lst in
  fisher_yates_shuffle arr;
  Array.to_list arr

(*labels files in a directory according to the right class *)
let label_files dir =
  List.flatten
    (List.map
       (fun (x, label) ->
         List.map
           (fun y -> (x ^ "/" ^ y, label))
           (Array.to_list (Sys.readdir x)))
       dir)
