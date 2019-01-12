open Core
open OUnit2
open Puzzle

let ae exp got _test_ctxt = assert_equal exp got ~printer:Int.to_string
let aeb exp got _test_ctxt = assert_equal exp got ~printer:Bool.to_string
let aes exp got _test_ctxt = assert_equal exp got ~printer:String.of_string
let aeo exp got _test_ctxt = assert_equal exp got ~printer:(fun o -> match o with None -> "None" | Some(v) -> Printf.sprintf "Some(%s)" v)

let tcmp (aa, ab) (ba, bb) = aa = ba && ab = bb
let tprinter (a, b) = Printf.sprintf "(%i, %i)" a b
let aet exp got _test_ctxt = assert_equal exp got ~cmp:tcmp ~printer:tprinter

let tbprinter (a, b) = Printf.sprintf "(%b, %b)" a b
let aetb exp got _test_ctxt = assert_equal exp got ~cmp:tcmp ~printer:tbprinter


let tests = [
     ("parse id" >:: aes "1" (parse "#1 @ 596,731: 11x27")#id);
     ("parse x" >:: ae 596 (parse "#1 @ 596,731: 11x27")#x);
     ("parse <" >:: ae 731 (parse "#1 @ 596,731: 11x27")#y);
     ("parse x" >:: ae 11 (parse "#1 @ 596,731: 11x27")#width);
     ("parse <" >:: ae 27 (parse "#1 @ 596,731: 11x27")#height);

     ("parse id" >:: aes "1164" (parse "#1164 @ 598,350: 12x13")#id);
     ("parse x" >:: ae 598 (parse "#1164 @ 598,350: 12x13")#x);
     ("parse <" >:: ae 350 (parse "#1164 @ 598,350: 12x13")#y);
     ("parse x" >:: ae 12 (parse "#1164 @ 598,350: 12x13")#width);
     ("parse <" >:: ae 13 (parse "#1164 @ 598,350: 12x13")#height);

     ("claim(x: 0, y: 0, w: 1, h: 1)#contains 0 0" >:: aeb true ((create_claim "1" [0; 0; 1; 1])#contains 0 0));
     ("claim(x: 0, y: 0, w: 1, h: 1)#contains 1 1" >:: aeb true ((create_claim "1" [0; 0; 1; 1])#contains 1 1));
     ("claim(x: 0, y: 0, w: 1, h: 1)#contains 1 2" >:: aeb false ((create_claim "1" [0; 0; 1; 1])#contains 1 2));
]

let () =
  run_test_tt_main ("Day 3.1 tests" >::: tests)
