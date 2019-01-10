open Core

type claim = < 
  id: string;
  x: int;
  y: int;
  width: int;
  height: int;
>

let parse line =
  let f = String.split_on_chars line ~on:['#'; '@'; ','; 'x'; ':']
    |> List.filter ~f:(Fn.non String.is_empty) in
  let n = List.drop f 1
    |> List.map ~f:(String.strip ~drop:(fun c -> c = ' '))
    |> List.map ~f:Int.of_string
  in
  object
    method id = List.nth_exn f 0 |> ((Fn.flip String.drop_suffix) 1)
    method x = List.nth_exn n 0
    method y = List.nth_exn n 1
    method width = List.nth_exn n 2
    method height = List.nth_exn n 3
  end