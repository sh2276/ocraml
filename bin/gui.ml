open Bogue
module W = Widget
module L = Layout
module T = Trigger
open Tsdl

let section_title s = L.flat_of_w [ W.label ~size:12 ~fg:Draw.(opaque grey) s ]

let hline width =
  let style = Style.(empty |> with_bg (color_bg Draw.(transp black))) in
  L.resident (W.box ~w:width ~h:1 ~style ())

let demo () =
  let width = 500 in

  (* Page 1: Upload Image *)
  let upload_title = section_title "Upload Image" in
  let upload_layout = L.tower ~margins:0 ~align:Draw.Center [ upload_title ] in
  let page1 = L.tower [ upload_layout; hline width ] in

  (* Page 2: Display Image *)
  let image_title = section_title "Image display" in
  let image_t_layout = L.tower ~margins:0 ~align:Draw.Center [ image_title ] in

  (*replace this image with uploaded image from page 1*)
  let image = W.image ~w:(width / 2) "./bin/handwritten_3.png" in
  let image_layout = L.tower ~margins:0 [ L.resident image ] in

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
    Label.set (W.get_label percent) " 0%"
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
          T.will_exit ev;
          Button.reset bw)
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
      [ buttons_title; L.flat_of_w [ button_reset; button_start ] ]
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
  let page3 = L.tower [ inference_layout; hline width ] in

  let tabs =
    Tabs.create ~slide:Avar.Right
      [ ("Upload Image", page1); ("Display Image", page2); ("Results", page3) ]
  in

  let board = Main.make [ c_slider; c_button ] [ tabs ] in
  Main.run board

let () = demo ()
