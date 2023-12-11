open Bogue
module W = Widget
module L = Layout
module T = Trigger
open Tsdl
open Ocraml
open Bimage

let section_title s = L.flat_of_w [ W.label ~size:12 ~fg:Draw.(opaque grey) s ]

let hline width =
  let style = Style.(empty |> with_bg (color_bg Draw.(transp black))) in
  L.resident (W.box ~w:width ~h:1 ~style ())

type image_num = { mutable num : int }
(* type image_name = { mutable name : string } *)

let cur_img_dir = ref "./none.png"

let _ =
  print_endline
    "Directory format: './[directory from ocraml base folder]/'. Make sure to \
     include the slash after the directory. Our default training directory is \
     ./strain/ and our default input directory is ./input/"

let _ =
  print_endline
    "If you press enter without entering anything, these default directories \
     will be inputted for you."

let _ =
  print_endline
    "What directory is your training data in? (This directory should only have \
     folders of training data, and each folder's name is the name of the label \
     of the contents within):"

let training_dir = input_line stdin

let _ =
  print_endline
    "What directory is your input data in? (This directory should only have \
     images that you want to run through the model): "

(* Get list of input file paths and make a list of input image vectors*)
let input_dir = input_line stdin

(* Set default training and input dir if input is empty *)
let training_dir = if training_dir = "" then "./strain/" else training_dir
let input_dir = if input_dir = "" then "./input/" else input_dir

(* Load files and preprocess *)
let _ = print_endline "Loading files (might take a bit)..."
let input_file_names = Loader.if_names input_dir

let input_images =
  Loader.to_vector_list input_file_names gray [ Expr.invert () ]

let file_name_to_image_map =
  List.map2 (fun name image -> (name, image)) input_file_names input_images

(* Get list of training file paths and make a list of labeled image vectors *)
(* Add labels to each of the files *)
let train_folders =
  List.map
    (fun x -> (training_dir ^ x, x))
    (Array.to_list (Sys.readdir training_dir))

let train_classes = List.map (fun (_, label) -> label) train_folders
let labeled_train_files = Loader.label_files train_folders
let unlabeled_train_files = List.map (fun (x, _) -> x) labeled_train_files
let train_labels = List.map (fun (_, label) -> label) labeled_train_files

let unlabeled_images =
  Loader.to_vector_list unlabeled_train_files gray [ Expr.invert () ]

let labeled_images =
  Loader.shuffle_list
    (List.map2
       (fun image label -> (image, label))
       unlabeled_images train_labels)

(* Initialize perceptron *)
exception BadTrainSet of string

let vector_size =
  let first_vector =
    match unlabeled_images with
    | h :: _ -> h
    | [] -> raise (BadTrainSet "first image is empty")
  in
  Vector.length first_vector

let perceptron = ref (Perceptron.create vector_size train_classes)
let epoch_count = ref 0

let demo () =
  let width = 500 in

  (* Page 1: OCRaml overview *)
  let overview_text =
    "OCRaml is an optical character recognition tool designed to recognize \
     handwritten digits. You have already uploaded your data. The next tab \
     contains controls to train our model. You can choose to train epochs, or \
     you can also reset the model. An untrained or reset model will predict to \
     be '1' The last tab displays all your input files as a button. Click one \
     to select it, and then click 'Predict' to see what the model thinks it \
     is."
  in
  let text_head =
    W.rich_text ~size:20 ~w:width ~h:30
      Text_display.(page [ bold (para "Welcome to OCRaml!") ])
  in
  let text = W.text_display ~w:width ~h:630 overview_text in
  let logo_title = section_title "OCRaml -> ORCa ml" in
  let logo = W.image ~w:(width / 2) "./bin/logo.png" in
  let logo_layout = L.tower ~margins:0 [ L.resident logo; logo_title ] in
  let page1 = L.tower [ L.resident text_head; L.resident text; logo_layout ] in

  (* Page 2: Training Controls *)
  let image_title =
    section_title "Choose the image you want to run the model on"
  in
  let image_t_layout = L.tower ~margins:0 ~align:Draw.Center [ image_title ] in

  let image () = W.image ~w:(width / 2) (Printf.sprintf "%s" !cur_img_dir) in

  let image_layout = L.tower_of_w [ image () ] in

  let make_button button_str =
    let button_start = W.button button_str ~border_radius:10 in
    let start_action b _ _ =
      let bw = W.get_button b in
      let state = Button.state bw in
      if state then (
        cur_img_dir := button_str;
        L.set_rooms image_layout [ L.tower_of_w [ image () ] ];
        Button.reset bw;
        print_endline ("Currently selected image" ^ !cur_img_dir))
    in
    let connected_button =
      W.connect ~priority:W.Replace button_start (image ()) start_action
        T.buttons_up
    in
    (button_start, connected_button)
  in

  let make_list file_names = List.map make_button file_names in
  let buttons_and_connections = make_list input_file_names in
  let input_button_list = List.map fst buttons_and_connections in
  let input_c_button_list = List.map snd buttons_and_connections in

  let epoch_label = W.label "Epochs trained: 0" in

  let buttons_title = section_title "Training Controls" in
  let button_reset = W.button ~border_radius:10 "Reset Training" in
  let update c n = W.set_text c n in
  let click _ =
    print_endline "Reset Training";
    perceptron := Perceptron.create vector_size train_classes;
    epoch_count := 0;
    update epoch_label ("Epochs trained: " ^ string_of_int !epoch_count)
  in
  W.on_click ~click button_reset;
  let button_epoch1 = W.button ~border_radius:10 "Train 1 epoch" in
  let button_epoch2 = W.button ~border_radius:10 "Train 10 epochs" in
  let button_epoch3 = W.button ~border_radius:10 "Train 100 epochs" in
  let update c n = W.set_text c n in
  let start_action_maker n b _ ev =
    let bw = W.get_button b in
    let state = Button.state bw in
    if state then (
      L.set_rooms image_layout [ L.tower_of_w [ image () ] ];
      T.will_exit ev;
      Button.reset bw;

      (* replace with perceptron inference *)
      print_endline "Start Training";
      perceptron := Perceptron.train 0.2 0. n labeled_images !perceptron |> fst;
      print_endline "Finished Training";
      epoch_count := !epoch_count + n;
      update epoch_label ("Epochs trained: " ^ string_of_int !epoch_count))
  in
  let create_c_button b n =
    W.connect ~priority:W.Replace b epoch_label (start_action_maker n)
      T.buttons_up
  in
  let c_button1 = create_c_button button_epoch1 1 in
  let c_button2 = create_c_button button_epoch2 10 in
  let c_button3 = create_c_button button_epoch3 100 in
  let buttons_layout =
    L.tower ~margins:0
      [
        buttons_title;
        L.flat_of_w
          [ button_reset; button_epoch1; button_epoch2; button_epoch3 ];
      ]
  in

  let top =
    L.flat ~align:Draw.Max ~margins:0 [ image_layout; Space.hfill () ]
  in
  L.set_width top width;

  let page2 = L.tower [ buttons_layout; L.tower_of_w [ epoch_label ] ] in

  (*page3*)
  let inference_title = section_title "Results" in
  let inference_layout =
    L.tower ~margins:0 ~align:Draw.Center [ inference_title ]
  in

  (* compute button *)
  let update c n = W.set_text c n in

  let prediction = ref "None" in

  let label = W.label "Output Label" in
  let count = W.label !prediction in
  let action _ =
    prediction :=
      Perceptron.predict
        (List.assoc !cur_img_dir file_name_to_image_map)
        !perceptron;
    update count !prediction;
    print_endline !prediction
  in
  let button = W.button ~action "Predict" in

  let page3 =
    L.tower
      [
        image_t_layout; hline width; top; L.tower_of_w input_button_list;
        inference_layout; hline width; L.tower_of_w [ label; count; button ];
      ]
  in

  let tabs =
    Tabs.create ~slide:Avar.Right
      [
        ("OCRaml Info", page1); ("Training Controls", page2);
        ("Run Model", page3);
      ]
  in

  let board =
    Main.make
      (c_button1 :: c_button2 :: c_button3 :: input_c_button_list)
      [ tabs ]
  in
  Main.run board

let () = demo ()
