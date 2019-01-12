open Core

type claim = < 
  id: string;
  x: int;
  y: int;
  width: int;
  height: int;
  contains: int -> int -> bool;
>

let create_claim id data =
  let get = List.nth_exn data in
  let x = get 0 in
  let y = get 1 in
  let width = get 2 in
  let height = get 3 in
  object (self)
    method id = id
    method x = x
    method y = y
    method width = width
    method height = height
    method contains x y = 
      self#x <= x && 
      self#y <= y && 
      x <= self#x + self#width && 
      y <= self#y + self#height
  end

let parse line =
  let f = String.split_on_chars line ~on:['#'; '@'; ','; 'x'; ':']
    |> List.filter ~f:(Fn.non String.is_empty)
  in
  let id = f
    |> (Fn.flip List.nth_exn) 0
    |> (Fn.flip String.drop_suffix) 1
  in
  let n = List.drop f 1
    |> List.map ~f:(String.strip ~drop:(fun c -> c = ' '))
    |> List.map ~f:Int.of_string
  in
  create_claim id n