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
     (* "distance" >:: ae 0 (distance "a" "a"); *)

]

let () =
  run_test_tt_main ("Day 1 tests" >::: tests)
