open OUnit2

let tests =
  "starter test suite"
  >::: [ ("assert true" >:: fun _ -> assert_equal true true) ]

let _ = run_test_tt_main tests
