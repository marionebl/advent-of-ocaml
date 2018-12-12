open Core

let compute l =
  List.sort l ~cmp:(fun item1 item2 -> String.compare item1 item2) |> 
  List.iter ~f:(fun i -> (Printf.printf "%s\n" i))

let solve filename = 
  In_channel.read_lines filename |> compute

let () = solve "input"