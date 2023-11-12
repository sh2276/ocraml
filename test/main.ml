(** Test plan: Since much of our system is built around somewhat untestable
    components (i.e.) GUI components, and results of neural network training
    (which is based on) some degree of randomization with regard to weight
    initalization, our testing plan seeks to thoroughly test the core components
    of our system. Specifically, we want to test the math modules developed for
    representing matrices, vectors, and other modules built for loading in data
    and processing images.

    Math modules will be tested manually. (UPDATE THIS) Test cases for math
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

let tests =
  "starter test suite"
  >::: [
         (*initialization tests*)
         ("initalize empty vector" >:: fun _ -> vector_init_tester [||]);
         (*vector addition tests*)
         ( "add two empty vectors" >:: fun _ ->
           vector_a_s_tester [||] [||] [||] Vector.add );
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
