open Bogue
module W = Widget

let () =
  let select_file = W.button "Select File" in
  let display_img = W.button "Display Image" in
  let inference = W.button "Inference" in 
  Layout.flat_of_w ~name:"OCRml Tutorial"
    ~align:Draw.Center [select_file; display_img; inference]
  |> Bogue.of_layout
  |> Bogue.run
