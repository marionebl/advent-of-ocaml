open Core

let stream_fold ~f ~init stream =
    let result = ref (None, init) in
    let exit = ref None in
    let () =
        while Option.is_none !exit do
            match f !result (Stream.next stream) with
                | (Some(r), _) -> exit := Some(r)
                | r -> result := r
        done 
    in
    Option.value_exn !exit

let cycle items =
    let buf = ref [] in
    let rec next _ =
      if !buf = [] then buf := items;
      match !buf with
        | h :: t -> (buf := t; Some h)
        | [] -> None in
    Stream.from next;;

let compute s = 
    let _compute (r, p) i = 
        match r with
            | Some(_) -> (r, p)
            | None ->
                let s = (i + List.hd_exn p) in
                ((List.find p ~f:(Int.equal s)), s :: p)
    in
    cycle s |>
    stream_fold ~f:_compute ~init:[0]

let solve filename = 
    In_channel.read_lines filename
    |> List.map ~f:Int.of_string
    |> compute