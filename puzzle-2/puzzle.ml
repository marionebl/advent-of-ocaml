open Core

let distance a b =
    String.foldi a ~init:0 ~f:(fun i acc c -> if c = (String.nget b i) then acc else acc + 1)

let find_match l i =
    List.find l ~f:(fun j -> (distance i j) = 1)

let common a b =
    String.foldi a ~init:[] ~f:(fun i acc c -> if c = (String.nget b i) then c :: acc else acc)
    |> List.rev
    |> String.of_char_list

let compute l =
    List.find_map l ~f:(fun i -> find_match l i |> Option.map ~f:(fun o -> (i, o)))
    |> (function
        | None -> ""
        | Some((a, b)) -> common a b
    )

let solve filename = 
  In_channel.read_lines filename |> compute
