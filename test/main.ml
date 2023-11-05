open OUnit2
open Ocraml

let a = Vector.init [| 1.; 2.; 3.; 4.; 5. |]

let tests =
  "starter test suite"
  >::: [
         ( "vector length of test vector" >:: fun _ ->
           assert_equal 5 (a |> Vector.length) );
       ]

let _ = run_test_tt_main tests
