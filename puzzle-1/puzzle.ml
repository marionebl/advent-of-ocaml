open Core

let sum = List.reduce_exn ~f:(+)

let parse = Int.of_string

let solve filename = 
    In_channel.read_lines filename
    |> List.map ~f:parse
    |> sum
