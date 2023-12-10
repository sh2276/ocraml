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

let img_name = ref "./none.png"

let _ =
  print_endline
    "Directory format: './[directory from ocraml base folder]/'. Make sure to \
     include the slash after the directory."

let _ =
  print_endline
    "What directory is your training data in? (This directory should only have \
     folders of training data, and each folder's name is the name of the label \
     of the contents within.) : "

let training_dir = input_line stdin
let _ = print_endline training_dir

let _ =
  print_endline
    "What directory is your input data in? (This directory should only have \
     images that you want to run through the model. ): "

(* Get list of input file paths *)
let input_dir = input_line stdin
let _ = print_endline input_dir
let input_dir = if input_dir = "" then "./uploads/" else input_dir
let _ = print_endline "Loading files..."

let input_files =
  List.map (fun x -> input_dir ^ x) (Array.to_list (Sys.readdir input_dir))

(* Get list of training file paths *)
let train_folders =
  List.map
    (fun x -> (training_dir ^ x, int_of_string x))
    (Array.to_list (Sys.readdir training_dir))

let train_files =
  List.map
    (fun x -> List.map (fun y -> x ^ "/" ^ y) (Array.to_list (Sys.readdir x)))
    train_folders

(* Shuffle the list *)
let _ = Random.self_init ()

let fisher_yates_shuffle arr =
  let len = Array.length arr in
  for i = 0 to len - 2 do
    let j = Random.int (len - i) + i in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done

let shuffle_list lst =
  let arr = Array.of_list lst in
  fisher_yates_shuffle arr;
  Array.to_list arr

let train_files = List.flatten train_files
let train_files = shuffle_list train_files
let _ = List.map print_string train_files
let image_matrix = Loader.to_matrix train_files gray []
(* let _ = print_endline (string_of_int (Matrix.num_rows image_matrix)) let _ =
   print_endline (string_of_int (Matrix.num_cols image_matrix)) *)

module Perceptron = Perceptron.Perceptron

let perceptron =
  Perceptron.create
    (Matrix.num_cols image_matrix)
    [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]

let train_n rate lst perceptron =
  let rec train_aux lst acc =
    match lst with
    | [] -> acc
    | (v, e) :: t ->
        let newp =
          print_endline (string_of_int e);
          Perceptron.update_weights rate e (Vector.init v) acc
        in
        train_aux t newp
  in
  train_aux lst perceptron

let demo () =
  let _ = List.map print_endline input_files in
  let width = 500 in

  (* Page 1: Upload Image *)
  let upload_title = section_title "Upload Image" in
  let upload_layout = L.tower ~margins:0 ~align:Draw.Center [ upload_title ] in
  let button_load = W.button ~border_radius:10 "Load" in
  let click _ = print_endline "clicked load" in
  W.on_click ~click button_load;
  let page1 =
    L.tower [ upload_layout; L.flat_of_w [ button_load ]; hline width ]
  in

  (* Page 2: Display Image *)
  let image_title = section_title "Image display" in
  let image_t_layout = L.tower ~margins:0 ~align:Draw.Center [ image_title ] in

  (* let cur_image_name = { name = "none.png" } in *)

  (* TODO: replace this image with uploaded image from page 1 *)
  let cur_image = { num = 0 } in

  let image () =
    (* W.image ~w:(width / 2) (Printf.sprintf "uploads/%u.png" cur_image.num) *)
    W.image ~w:(width / 2) (Printf.sprintf "%s" !img_name)
  in

  let image_layout = L.tower_of_w [ image () ] in

  let make_button button_str =
    let button_start = W.button button_str in
    let start_action b _ _ =
      let bw = W.get_button b in
      let state = Button.state bw in
      if state then (
        img_name := button_str;
        L.set_rooms image_layout [ L.tower_of_w [ image () ] ];
        Button.reset bw;
        print_endline !img_name)
    in
    let connected_button =
      W.connect ~priority:W.Replace button_start (image ()) start_action
        T.buttons_up
    in
    (button_start, connected_button)
  in

  (* W.button button_str in *)
  let make_list file_names = List.map make_button file_names in
  let buttons_and_connections = make_list input_files in
  let input_button_list = List.map fst buttons_and_connections in
  let input_c_button_list = List.map snd buttons_and_connections in

  let slider_title = section_title "Progress bar" in
  let slider = W.slider ~kind:Slider.HBar 100 in
  let percent = W.label "    0%" in
  let set_percent w x = Label.set (W.get_label w) (Printf.sprintf "%u%%" x) in
  let action w1 w2 _ =
    let x = Slider.value (W.get_slider w1) in
    set_percent w2 x
  in
  let events =
    List.flatten
      [ T.buttons_down; T.buttons_up; T.pointer_motion; [ Sdl.Event.key_down ] ]
  in
  let c_slider = W.connect slider percent action events in
  let slider_l = L.resident ~background:L.theme_bg slider in
  let slider_bar = L.flat ~align:Draw.Center [ slider_l; L.resident percent ] in
  let slider_layout = L.tower ~margins:0 [ slider_title; slider_bar ] in

  let buttons_title = section_title "Inference Buttons" in
  let button_reset = W.button ~border_radius:10 "Reset" in
  let click _ =
    Slider.set (W.get_slider slider) 0;
    Label.set (W.get_label percent) " 0%";
    print_endline "clicked button"
  in
  W.on_click ~click button_reset;
  let button_start =
    W.button ~border_radius:10 ~kind:Button.Switch "Start computing"
  in
  let start_action b s ev =
    let bw = W.get_button b in
    let sw = W.get_slider s in
    let state = Button.state bw in

    if state then
      let rec loop () =
        let x = Slider.value sw in
        if x >= 100 || T.should_exit ev then (
          cur_image.num <- cur_image.num + 1;
          L.set_rooms image_layout [ L.tower_of_w [ image () ] ];
          T.will_exit ev;
          Button.reset bw;

          (* replace with perceptron inference *)
          print_endline "pressed start computing")
        else (
          Slider.set sw (x + 1);
          set_percent percent (x + 1);
          W.update s;
          T.nice_delay ev 0.1;
          loop ())
      in
      loop ()
    else W.update percent
  in
  let c_button =
    W.connect ~priority:W.Replace button_start slider start_action T.buttons_up
  in
  let buttons_layout =
    L.tower ~margins:0
      [
        buttons_title;
        L.flat_of_w [ button_reset; button_start ];
        L.tower_of_w input_button_list;
      ]
  in

  let top =
    L.flat ~align:Draw.Max ~margins:0 [ image_layout; Space.hfill () ]
  in
  L.set_width top width;

  let page2 =
    L.tower
      [
        image_t_layout;
        hline width;
        top;
        buttons_layout;
        hline width;
        slider_layout;
        hline width;
      ]
  in

  (*page3*)
  let inference_title = section_title "Results" in
  let inference_layout =
    L.tower ~margins:0 ~align:Draw.Center [ inference_title ]
  in

  (* compute button *)
  let update c n = W.set_text c (string_of_int n) in

  let out = ref 0 in

  let compute () = incr out in

  let label = W.label "Output Label" in
  let count = W.label "0" in
  let action _ =
    compute ();
    update count !out;
    print_endline (string_of_int !out)
  in
  let button = W.button ~action "Predict" in

  let page3 =
    L.tower
      [ inference_layout; hline width; L.tower_of_w [ label; count; button ] ]
  in

  let tabs =
    Tabs.create ~slide:Avar.Right
      [ ("Upload Image", page1); ("Display Image", page2); ("Results", page3) ]
  in

  let board =
    Main.make ([ c_slider; c_button ] @ input_c_button_list) [ tabs ]
  in
  Main.run board

let () = demo ()
