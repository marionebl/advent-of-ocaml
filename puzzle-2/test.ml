open Core
open OUnit2
open Puzzle

let ae exp got _test_ctxt = assert_equal exp got ~printer:Int.to_string

let tests = [
     "compute" >:: ae 0 (compute [1; -1]);
     "compute" >:: ae 10 (compute [3; 3; 4; -2; -4]);
     "compute" >:: ae 5 (compute [-6; +3; +8; +5; -6]); 
     "compute" >:: ae 14 (compute [+7; +7; -2; -7; -4]);
     "solve" >:: ae 394 (solve "input");
]

let () =
  run_test_tt_main ("Day 1 tests" >::: tests)
