open Base
open OUnit2
open Puzzle

let lprinter l = Printf.sprintf "[%s]" (String.concat ~sep:";" l)
let ael exp got _test_ctxt = assert_equal exp got ~cmp:(List.equal ~equal:String.equal) ~printer:lprinter

let tests = [
     "sort_input" >:: ael ["1"; "2"; "3"] (sort_input ["2"; "1"; "3"])
]

let () =
  run_test_tt_main ("Day 1 tests" >::: tests)
