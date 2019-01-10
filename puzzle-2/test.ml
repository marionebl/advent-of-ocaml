open Core
open OUnit2
open Puzzle

let ae exp got _test_ctxt = assert_equal exp got ~printer:Int.to_string
let aes exp got _test_ctxt = assert_equal exp got ~printer:String.of_string
let aeo exp got _test_ctxt = assert_equal exp got ~printer:(fun o -> match o with None -> "None" | Some(v) -> Printf.sprintf "Some(%s)" v)

let tcmp (aa, ab) (ba, bb) = aa = ba && ab = bb
let tprinter (a, b) = Printf.sprintf "(%i, %i)" a b
let aet exp got _test_ctxt = assert_equal exp got ~cmp:tcmp ~printer:tprinter

let tbprinter (a, b) = Printf.sprintf "(%b, %b)" a b
let aetb exp got _test_ctxt = assert_equal exp got ~cmp:tcmp ~printer:tbprinter


let tests = [
     "distance" >:: ae 0 (distance "a" "a");
     "distance" >:: ae 1 (distance "a" "b");
     "distance" >:: ae 2 (distance "aa" "bb");
     "common" >:: aes "" (common "a" "b");
     "common" >:: aes "a" (common "aa" "ab");
     "common" >:: aes "b" (common "ab" "cb");
     "find_match" >:: aeo None (find_match [] "");
     "find_match" >:: aeo None (find_match ["a"] "a");
     "find_match" >:: aeo None (find_match ["aa"; "bb"] "aa");
     "find_match" >:: aeo (Some "ab") (find_match ["aa"; "bb"; "ab"] "aa");
     "compute" >:: aes "a" (compute ["aa"; "bb"; "ab"]);
     "solve" >:: aes "megsdlpulxvinkatfoyzxcbvq" (solve "input");
]

let () =
  run_test_tt_main ("Day 1 tests" >::: tests)
