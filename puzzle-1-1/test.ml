open Core
open OUnit2
open Puzzle

let ae exp got _test_ctxt = assert_equal exp got ~printer:Int.to_string

let tests = [
     "sum [1; 1]" >:: ae 2 (sum [1; 1]);
     "sum [1; -10; +30; -7]" >:: ae 14 (sum [1; -10; 30; -7]);
     "parse 1" >:: ae 1 (parse "1");
     "parse -1" >:: ae (-1) (parse "-1");
     "solve" >:: ae 520 (solve "input");
]

let () =
  run_test_tt_main ("Day 1 tests" >::: tests)
