(** Test plan: Since much of our system is built around somewhat untestable
    components (i.e.) GUI components, and results of neural network training
    (which is based on) some degree of randomization with regard to weight
    initalization, our testing plan seeks to thoroughly test the core components
    of our system. Specifically, we want to test the math modules developed for
    representing matrices, vectors, and other modules built for loading in data
    and processing images.

    Math modules will be tested manually. Test cases for math
    modules will be developed using both glass and black-box testing, as well as
    some randomized testing. 

    This testing approach demonstrates the correctness of the system, as it
    ensures that some untestable features of our system that depend on core,
    testable modules, have dependencies that are functioning as expected. *)

open OUnit2
open Ocraml
open Vector

(*Tester functions*)

(** Function to test if a vector was initialized correctly. *)
let vector_init_tester (in1 : float array) =
  let vect = init in1 in
  let arr_from_vector = to_array vect in
  assert_equal
    ~printer:(fun arr ->
      Printf.sprintf "[|%s|]"
        (String.concat "; " Array.(to_list (map string_of_float arr))))
    ~msg:"Vector not initalized properly." in1 arr_from_vector

(** Function to test vector add and subtract operations. *)
let vector_a_s_tester (out : float array) (in1 : float array)
    (in2 : float array) (op : Vector.t -> Vector.t -> Vector.t) =
  let v1 = init in1 in
  let v2 = init in2 in
  let arr_from_op = to_array (op v1 v2) in
  assert_equal
    ~printer:(fun arr ->
      Printf.sprintf "[|%s|]"
        (String.concat "; " Array.(to_list (map string_of_float arr))))
    ~msg:"Vector operation failed." out arr_from_op

(** Function to test vector scalar multiplication operation. *)
let vector_s_mult_tester (out : float array) (in1 : float) (in2 : float array) =
  let vect = init in2 in
  let arr_from_op = to_array (scalar_mult in1 vect) in
  let arr_from_infix = to_array (in1 * vect) in
  let ae arr =
    assert_equal
      ~printer:(fun arr ->
        Printf.sprintf "[|%s|]"
          (String.concat "; " Array.(to_list (map string_of_float arr))))
      ~msg:"Vector scalar muliplication failed." out arr
  in
  ae arr_from_op;
  ae arr_from_infix

(** Function to test vector dot product operation. *)
let vector_dprod_tester (out : float) (in1 : float array) (in2 : float array) =
  let v1 = init in1 in
  let v2 = init in2 in
  let float_from_op = dot_prod v1 v2 in
  let float_from_infix = v1 @ v2 in
  let ae res =
    assert_equal ~printer:string_of_float ~msg:"Vector dot produt failed." out
      res
  in
  ae float_from_op;
  ae float_from_infix

(** Function to test the length of a vector. *)
let vector_length_tester (in1 : float array) =
  let vect = init in1 in
  let vec_len = length vect in
  let arr_len = Array.length in1 in
  assert_equal ~printer:string_of_int ~msg:"Vector length test failed." vec_len
    arr_len

(** Vectors to be used for test cases *)
let v5 = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]

let v10 = [| 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9; 1.0 |]

let v15 =
  [|
    -1.0;
    -2.0;
    -3.0;
    -4.0;
    -5.0;
    0.0;
    1.0;
    2.0;
    3.0;
    4.0;
    5.0;
    0.0;
    -1.0;
    -2.0;
    -3.0;
  |]

let v20 =
  [|
    2.5;
    3.5;
    4.5;
    5.5;
    6.5;
    7.5;
    8.5;
    9.5;
    10.5;
    11.5;
    12.5;
    13.5;
    14.5;
    15.5;
    16.5;
    17.5;
    18.5;
    19.5;
    20.5;
    21.5;
  |]

let v5_2 = [| 0.5; 1.5; 2.5; 3.5; 4.5 |]
(*let v5_3 = [| -0.5; -1.5; -2.5; -3.5; -4.5 |] let v10_2 = [| 10.0; 9.0; 8.0;
  7.0; 6.0; 5.0; 4.0; 3.0; 2.0; 1.0 |] let v10_3 = [| 0.01; 0.02; 0.03; 0.04;
  0.05; 0.06; 0.07; 0.08; 0.09; 0.1 |] let v10_4 = [| -10.0; -9.0; -8.0; -7.0;
  -6.0; -5.0; -4.0; -3.0; -2.0; -1.0 |]

  let v20_2 = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0; 11.0; 12.0;
  13.0; 14.0; 15.0; 16.0; 17.0; 18.0; 19.0; 20.0; |]*)

let tests =
  "starter test suite"
  >::: [
         (*initialization tests*)
         ( "initialize vector of length 5 with specific values" >:: fun _ ->
           vector_init_tester v5 );
         ( "initialize vector of length 10 with specific values" >:: fun _ ->
           vector_init_tester v10 );
         ( "initialize vector of length 15 with specific values" >:: fun _ ->
           vector_init_tester v15 );
         ( "initialize vector of length 20 with specific values" >:: fun _ ->
           vector_init_tester v20 );
         (*vector addition tests*)
         ( "add two empty vectors" >:: fun _ ->
           vector_a_s_tester [||] [||] [||] Vector.add );
         ( "add two vectors of length 5" >:: fun _ ->
           vector_a_s_tester [|2.; 4.; 6.; 8.; 10.|] v5 v5 Vector.add );
         ( "add two vectors of length 10" >:: fun _ ->
           vector_a_s_tester
           [|0.2; 0.4; 0.6; 0.8; 1.; 1.2; 1.4; 1.6; 1.8; 2.|]
             v10 v10 Vector.add );
         ( "add two vectors of length 15" >:: fun _ ->
           vector_a_s_tester
             [|
               -2.;
               -4.;
               -6.;
               -8.;
               -10.;
               0.;
               2.;
               4.;
               6.;
               8.;
               10.;
               0.;
               -2.;
               -4.;
               -6.;
             |]
             v15 v15 Vector.add );
         ( "add two vectors of length 20" >:: fun _ ->
           vector_a_s_tester
             [|
               5.;
               7.;
               9.;
               11.;
               13.;
               15.;
               17.;
               19.;
               21.;
               23.;
               25.;
               27.;
               29.;
               31.;
               33.;
               35.;
               37.;
               39.;
               41.;
               43.;
             |]
             v20 v20 Vector.add );
         ( "add two vectors of length 5 with different values" >:: fun _ ->
           vector_a_s_tester [| 1.5; 3.5; 5.5; 7.5; 9.5 |] v5 v5_2 Vector.add );
         (*vector subtraction tests*)
         ( "subtract two empty vectors" >:: fun _ ->
           vector_a_s_tester [||] [||] [||] Vector.sub );
         (*vector scalar multiplication tests*)
         ( "multiply empty vector by 0.0" >:: fun _ ->
           vector_s_mult_tester [||] 0.0 [||] );
         (*vector dot product tests*)
         ( "multiply two singleton vectors" >:: fun _ ->
           vector_dprod_tester 1.0 [| 1.0 |] [| 1.0 |] );
         (*vector length tests*)
         ("length of empty vector" >:: fun _ -> vector_length_tester [||]);
       ]

let _ = run_test_tt_main tests
