(** Test plan: Since much of our system is built around somewhat untestable
    components such as GUI components and results of neural network training
    which are based on some degree of randomization with regard to weight
    initalization, our testing plan seeks to thoroughly test the core components
    of our system. Specifically, we want to test the math modules developed for
    representing matrices, vectors, and other modules built for loading in data
    and processing images.

    Math modules will be tested manually. Test cases for math modules will be
    developed using both glass and black-box testing. The core perceptron
    algorithm will be tested using glass box tests on simple logic gates in 
    order to demonstrate functionality. Other modules will be tested with of 
    user-testing (i.e. testing through the GUI).

    This testing approach demonstrates the correctness of the system, as it
    ensures that some untestable features of our system that depend on core,
    testable modules, have dependencies that are functioning as expected. *)

open OUnit2
open Ocraml
open Vector

(*==============================================================================
                             VECTOR TEST SUITE
==============================================================================*)
(** Vector Test Functions *)

(** Function to test if a vector was initialized correctly. *)
let vector_init_tester (in1 : float array) =
  let vect = Vector.init in1 in
  let arr_from_vector = Vector.to_array vect in
  assert_equal
    ~printer:(fun arr ->
      Printf.sprintf "[|%s|]"
        (String.concat "; " Array.(to_list (map string_of_float arr))))
    ~msg:"Vector not initalized properly." in1 arr_from_vector

(** Function to test vector add and subtract operations. *)
let vector_a_s_tester (out : float array) (in1 : float array)
    (in2 : float array) (op : Vector.t -> Vector.t -> Vector.t) =
  let v1 = Vector.init in1 in
  let v2 = Vector.init in2 in
  let arr_from_op = Vector.to_array (op v1 v2) in
  assert_equal
    ~printer:(fun arr ->
      Printf.sprintf "[|%s|]"
        (String.concat "; " Array.(to_list (map string_of_float arr))))
    ~msg:"Vector operation failed." out arr_from_op

(** Function to test vector scalar multiplication operation. *)
let vector_s_mult_tester (out : float array) (in1 : float) (in2 : float array) =
  let vect = Vector.init in2 in
  let arr_from_op = Vector.to_array (scalar_mult in1 vect) in
  let arr_from_infix = Vector.to_array Vector.(in1 * vect) in
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
  let v1 = Vector.init in1 in
  let v2 = Vector.init in2 in
  let float_from_op = dot_prod v1 v2 in
  let float_from_infix = Vector.(v1 @ v2) in
  let ae res =
    assert_equal ~printer:string_of_float ~msg:"Vector dot product failed." out
      res
  in
  ae float_from_op;
  ae float_from_infix

(** Function to test the length of a vector. *)
let vector_length_tester (in1 : float array) =
  let vect = Vector.init in1 in
  let vec_len = Vector.length vect in
  let arr_len = Array.length in1 in
  assert_equal ~printer:string_of_int ~msg:"Vector length test failed." vec_len
    arr_len

(** Function to test the argmax of a vector *)
let vector_argmax_tester (out : int) (in1 : float array) =
  let vect = Vector.init in1 in
  let idx = Vector.argmax vect in
  assert_equal ~printer:string_of_int out idx

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
let v5_3 = [| -0.5; -1.5; -2.5; -3.5; -4.5 |]
let v10_2 = [| 10.0; 9.0; 8.0; 7.0; 6.0; 5.0; 4.0; 3.0; 2.0; 1.0 |]
let v10_3 = [| 0.01; 0.02; 0.03; 0.04; 0.05; 0.06; 0.07; 0.08; 0.09; 0.1 |]

let v20_2 =
  [|
    1.0;
    2.0;
    3.0;
    4.0;
    5.0;
    6.0;
    7.0;
    8.0;
    9.0;
    10.0;
    11.0;
    12.0;
    13.0;
    14.0;
    15.0;
    16.0;
    17.0;
    18.0;
    19.0;
    20.0;
  |]

(** Vector test cases *)
let vec_tests =
  [
    (* initialization tests *)
    ( " initialize vector of length 5 with specific values" >:: fun _ ->
      vector_init_tester v5 );
    ( " initialize vector of length 10 with specific values" >:: fun _ ->
      vector_init_tester v10 );
    ( " initialize vector of length 15 with specific values" >:: fun _ ->
      vector_init_tester v15 );
    ( " initialize vector of length 20 with specific values" >:: fun _ ->
      vector_init_tester v20 );
    (* vector addition tests *)
    ( " add two empty vectors" >:: fun _ ->
      vector_a_s_tester [||] [||] [||] Vector.add );
    ( " add two vectors of length 5" >:: fun _ ->
      vector_a_s_tester [| 2.; 4.; 6.; 8.; 10. |] v5 v5 Vector.add );
    ( " add two vectors of length 10" >:: fun _ ->
      vector_a_s_tester
        [| 0.2; 0.4; 0.6; 0.8; 1.; 1.2; 1.4; 1.6; 1.8; 2. |]
        v10 v10 Vector.add );
    ( " add two vectors of length 15" >:: fun _ ->
      vector_a_s_tester
        [|
          -2.; -4.; -6.; -8.; -10.; 0.; 2.; 4.; 6.; 8.; 10.; 0.; -2.; -4.; -6.;
        |]
        v15 v15 Vector.add );
    ( " add two vectors of length 20" >:: fun _ ->
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
    ( " add two vectors of length 5 with different values" >:: fun _ ->
      vector_a_s_tester [| 1.5; 3.5; 5.5; 7.5; 9.5 |] v5 v5_2 Vector.add );
    (*vector subtraction tests*)
    ( " subtract two empty vectors" >:: fun _ ->
      vector_a_s_tester [||] [||] [||] Vector.sub );
    ( " subtract two vectors of length 5" >:: fun _ ->
      vector_a_s_tester [| 0.5; 0.5; 0.5; 0.5; 0.5 |] v5 v5_2 Vector.sub );
    ( " subtract two vectors of length 10" >:: fun _ ->
      vector_a_s_tester
        [| -9.9; -8.8; -7.7; -6.6; -5.5; -4.4; -3.3; -2.2; -1.1; 0.0 |]
        v10 v10_2 Vector.sub );
    ( " subtract two vectors of length 15" >:: fun _ ->
      vector_a_s_tester
        [| 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0. |]
        v15 v15 Vector.sub );
    ( " subtract two vectors of length 20" >:: fun _ ->
      vector_a_s_tester
        [|
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
          1.5;
        |]
        v20 v20_2 Vector.sub );
    ( " subtract two vectors of length 5 with different values" >:: fun _ ->
      vector_a_s_tester [| -0.5; -0.5; -0.5; -0.5; -0.5 |] v5_2 v5 Vector.sub );
    (*vector scalar multiplication tests*)
    ( " multiply empty vector by 0.0" >:: fun _ ->
      vector_s_mult_tester [||] 0.0 [||] );
    ( " multiply vector of length 5 by 2.0" >:: fun _ ->
      vector_s_mult_tester [| 2.0; 4.0; 6.0; 8.0; 10.0 |] 2.0 v5 );
    ( " multiply vector of length 15 by 0.5" >:: fun _ ->
      vector_s_mult_tester
        [|
          -0.5;
          -1.0;
          -1.5;
          -2.0;
          -2.5;
          0.0;
          0.5;
          1.0;
          1.5;
          2.0;
          2.5;
          0.0;
          -0.5;
          -1.0;
          -1.5;
        |]
        0.5 v15 );
    (* vector dot product tests *)
    ( "dot product of two singleton vectors" >:: fun _ ->
      vector_dprod_tester 1.0 [| 1.0 |] [| 1.0 |] );
    ( "dot product of two length 5 vectors" >:: fun _ ->
      vector_dprod_tester 47.5 v5 v5_2 );
    ( "dot product of two length 10 vectors" >:: fun _ ->
      vector_dprod_tester 0.385 v10 v10_3 );
    ( "dot product of two length 15 vectors" >:: fun _ ->
      vector_dprod_tester 124.0 v15 v15 );
    ( "dot product of two length 20 vectors" >:: fun _ ->
      vector_dprod_tester 3185.0 v20 v20_2 );
    (* vector length tests *)
    ("length of empty vector" >:: fun _ -> vector_length_tester [||]);
    ("length of vector of length 5" >:: fun _ -> vector_length_tester v5);
    ("length of vector of length 10" >:: fun _ -> vector_length_tester v10);
    ("length of vector of length 15" >:: fun _ -> vector_length_tester v15);
    ("length of vector of length 20" >:: fun _ -> vector_length_tester v20);
    ( "length of vector of length 5 with different values" >:: fun _ ->
      vector_length_tester v5_2 );
    ( "length of vector of length 10 with different values" >:: fun _ ->
      vector_length_tester v10_2 );
    ( "length of vector of length 20 with different values" >:: fun _ ->
      vector_length_tester v20_2 );
    ( "length of vector of length 5 with negative values" >:: fun _ ->
      vector_length_tester v5_3 );
    ( "length of vector of length 10 with negative values" >:: fun _ ->
      vector_length_tester v10_3 );
    (* vector argmax tests *)
    ("argmax of vector of length 5" >:: fun _ -> vector_argmax_tester 4 v5);
    ("argmax of vector of length 10" >:: fun _ -> vector_argmax_tester 9 v10);
    ( "argmax of vector of length 15 with negative values" >:: fun _ ->
      vector_argmax_tester 10 v15 );
    ( "argmax of vector with the same values" >:: fun _ ->
      vector_argmax_tester 0 [| 1.; 1. |] );
  ]

(*==============================================================================
                             MATRIX TEST SUITE
==============================================================================*)

(**Matrix test functions*)

(** Function to convert a row of a matrix to a string. *)
let row_to_string row =
  "[|"
  ^ String.concat "; " (Array.to_list (Array.map string_of_float row))
  ^ "|]"

(** Function to test correct initialization of the matrix. *)
let matrix_init_tester (in1 : float array array) =
  let mat = Matrix.init in1 in
  let arr_from_matrix = Matrix.to_array mat in
  assert_equal
    ~printer:(fun mat ->
      Printf.sprintf "[| %s |]"
        (String.concat "; " (Array.to_list (Array.map row_to_string mat))))
    ~msg:"Matrix not initialized properly." in1 arr_from_matrix

(** Function to test num_rows and num_cols simultaneously. *)
let matrix_dim_tester (mat : float array array) (r : int) (c : int) =
  let mat = Matrix.init mat in
  let actual_rows = Matrix.num_rows mat in
  let actual_cols = Matrix.num_cols mat in
  assert_equal ~printer:string_of_int ~msg:"Number of rows mismatch." r
    actual_rows;
  assert_equal ~printer:string_of_int ~msg:"Number of columns mismatch." c
    actual_cols

(** Function to test matrix-vector multiplication. *)
let mat_vec_prod_tester (out : float array) (mat : float array array)
    (vec : float array) =
  let mat = Matrix.init mat in
  let vec = Vector.init vec in
  let prod = Vector.to_array (Matrix.mat_vec_prod mat vec) in
  let prod_2 = Vector.to_array Matrix.(mat @ vec) in
  let ae o i =
    assert_equal
      ~printer:(fun arr ->
        Printf.sprintf "[| %s |]"
          (String.concat "; " Array.(to_list (map string_of_float arr))))
      ~msg:"Vector operation failed." o i
  in
  ae out prod;
  ae out prod_2

(** Function to test matrix-matrix multiplication. *)
let mat_mat_prod_tester (out : float array array) (mat1 : float array array)
    (mat2 : float array array) =
  let mat1 = Matrix.init mat1 in
  let mat2 = Matrix.init mat2 in
  let prod = Matrix.to_array (Matrix.mat_mat_prod mat1 mat2) in
  let prod_2 = Matrix.to_array Matrix.(mat1 * mat2) in
  let ae o i =
    assert_equal
      ~printer:(fun m ->
        Printf.sprintf "[| %s |]"
          (String.concat "; " (Array.to_list (Array.map row_to_string m))))
      ~msg:"Matrix operation failed." o i
  in
  ae out prod;
  ae out prod_2

let mat_mat_add_tester (out : float array array) (mat1 : float array array)
    (mat2 : float array array) =
  let mat1 = Matrix.init mat1 in
  let mat2 = Matrix.init mat2 in
  let sum = Matrix.(mat_mat_add mat1 mat2 |> to_array) in
  assert_equal
    ~printer:(fun m ->
      Printf.sprintf "[| %s |]"
        (String.concat "; " (Array.to_list (Array.map row_to_string m))))
    ~msg:"Matrix operation failed." out sum

(** Function to test matrix transpose. *)
let matrix_transpose_tester (mat : float array array) (out : float array array)
    =
  let mat = Matrix.init mat in
  let trans = Matrix.to_array (Matrix.transpose mat) in
  let ae o i =
    assert_equal
      ~printer:(fun m ->
        Printf.sprintf "[| %s |]"
          (String.concat "; " (Array.to_list (Array.map row_to_string m))))
      ~msg:"Matrix transpose failed." o i
  in
  ae out trans

(* Test cases for the Matrix module *)
let m3_3 = [| [| 1.0; 2.0; 3.0 |]; [| 4.0; 5.0; 6.0 |]; [| 7.0; 8.0; 9.0 |] |]
let m3_3_2 = [| [| 2.0; 0.0; 1.0 |]; [| 1.0; 0.0; 1.0 |]; [| 1.0; 1.0; 0.0 |] |]

let m3_4 =
  [|
    [| 1.0; 2.0; 3.0; 4.0 |];
    [| 5.0; 6.0; 7.0; 8.0 |];
    [| 9.0; 10.0; 11.0; 12.0 |];
  |]

let m4_2 = [| [| 4.0; 1.0 |]; [| 4.0; 2.0 |]; [| 2.0; 3.0 |]; [| 4.0; 5.0 |] |]
let v3 = [| 1.0; 2.0; 3.0 |]

(** Matrix Test Cases *)
let mat_tests =
  [
    (* Initialization tests *)
    ("initialize empty matrix" >:: fun _ -> matrix_init_tester [||]);
    ("initialize 3x3 matrix " >:: fun _ -> matrix_init_tester m3_3);
    ("initalize non square matrix" >:: fun _ -> matrix_init_tester m4_2);
    (* Matrix dimensions test *)
    ("check 3x3 matrix dimensions" >:: fun _ -> matrix_dim_tester m3_3 3 3);
    ("check 3x4 matrix dimensions" >:: fun _ -> matrix_dim_tester m3_4 3 4);
    ("check 4x2 matrix dimensions" >:: fun _ -> matrix_dim_tester m4_2 4 2);
    (* Matrix-vector multiplication tests *)
    ( "multiply 3x3 matrix by vector" >:: fun _ ->
      mat_vec_prod_tester [| 14.0; 32.0; 50.0 |] m3_3 v3 );
    (* Matrix-matrix multiplication tests *)
    ( "multiply two matrices" >:: fun _ ->
      mat_mat_prod_tester
        [| [| 7.0; 3.0; 3.0 |]; [| 19.0; 6.0; 9.0 |]; [| 31.0; 9.0; 15.0 |] |]
        m3_3 m3_3_2 );
    ( "multiply 3*4 and 4*2 matrix" >:: fun _ ->
      mat_mat_prod_tester
        [| [| 34.0; 34.0 |]; [| 90.0; 78.0 |]; [| 146.0; 122.0 |] |]
        m3_4 m4_2 );
    (* Matrix addition tests *)
    ( "add 2x2 matrices" >:: fun _ ->
      mat_mat_add_tester
        [| [| 1.0; 2.0 |]; [| 3.0; 4.0 |] |]
        [| [| 0.0; 1.0 |]; [| 1.0; 2.0 |] |]
        [| [| 1.0; 1.0 |]; [| 2.0; 2.0 |] |] );
    (* Matrix transpose test *)
    ( "transpose 3x3 matrix" >:: fun _ ->
      matrix_transpose_tester m3_3
        [| [| 1.0; 4.0; 7.0 |]; [| 2.0; 5.0; 8.0 |]; [| 3.0; 6.0; 9.0 |] |] );
    ( "transpose 3x3 matrix m3_3_2" >:: fun _ ->
      matrix_transpose_tester m3_3_2
        [| [| 2.0; 1.0; 1.0 |]; [| 0.0; 0.0; 1.0 |]; [| 1.0; 1.0; 0.0 |] |] );
    ( "transpose 3x4 matrix m3_4" >:: fun _ ->
      matrix_transpose_tester m3_4
        [|
          [| 1.0; 5.0; 9.0 |];
          [| 2.0; 6.0; 10.0 |];
          [| 3.0; 7.0; 11.0 |];
          [| 4.0; 8.0; 12.0 |];
        |] );
    ( "transpose 4x2 matrix m4_2" >:: fun _ ->
      matrix_transpose_tester m4_2
        [| [| 4.0; 4.0; 2.0; 4.0 |]; [| 1.0; 2.0; 3.0; 5.0 |] |] );
    (* Conversion to array test *)
    ("convert 3x3 matrix to array" >:: fun _ -> matrix_init_tester m3_3);
    ("convert 3x4 matrix to array" >:: fun _ -> matrix_init_tester m3_4);
    ("convert 4x2 matrix to array" >:: fun _ -> matrix_init_tester m4_2);
  ]

let loader_tests = []

(*==============================================================================
                            PERCEPTRON TEST SUITE
==============================================================================*)

let bool_perceptron = Perceptron.Perceptron.create 2 [ 0; 1 ]

let and_list =
  [
    ([| 1.0; 1.0 |], 1);
    ([| 0.0; 0.0 |], 0);
    ([| 1.0; 0.0 |], 0);
    ([| 0.0; 1.0 |], 0);
  ]

let train_n rate lst perceptron =
  let rec train_aux lst acc =
    match lst with
    | [] -> acc
    | (v, e) :: t ->
        let newp =
          print_endline (string_of_int e);
          Perceptron.Perceptron.update_weights rate e (Vector.init v) acc
        in
        train_aux t newp
  in
  train_aux lst perceptron

let perceptron_and_test (out : int) (in1 : Vector.t) =
  let result = train_n 0.1 and_list bool_perceptron in
  let prediction = Perceptron.Perceptron.predict in1 result in
  assert_equal out prediction

let perceptron_tests =
  [
    ( "and perceptron" >:: fun _ ->
      perceptron_and_test 1 (Vector.init [| 1.0; 1.0 |]) );
  ]

let suite =
  "ocraml test suite"
  >::: List.flatten [ vec_tests; mat_tests; perceptron_tests; loader_tests ]

let _ = run_test_tt_main suite
