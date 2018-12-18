open Core

let sort_input l = List.sort (String.compare) l

let solve filename = 
  In_channel.read_lines filename |> sort_input
