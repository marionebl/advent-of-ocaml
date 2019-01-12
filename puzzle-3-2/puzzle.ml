open Core

module Coords = struct
  include Tuple.Make (Int) (Int)
  include Tuple.Comparable (Int) (Int)
end

module Claim = struct
  type t = {id: int; x: int; y: int; w: int; h: int}

  let of_string s = Scanf.sscanf s "#%d @ %d,%d: %dx%d" (fun id x y w h -> {id; x; y; w; h} )

  let x_axis t = List.range t.x (t.x + t.w)

  let y_axis t = List.range t.y (t.y + t.h)

  let coords t =
    List.cartesian_product (x_axis t) (y_axis t)

  let stake ~map ~claim =
    List.fold (coords claim) 
      ~init:map 
      ~f:(fun claims coord -> Coords.Map.update claims coord ~f:(Option.value_map ~default:1 ~f:(fun v -> v + 1)))

  let safe ~map ~claim =
    (coords claim) 
    |> List.filter_map ~f:(fun claim -> Coords.Map.find map claim)
    |> List.for_all ~f:(fun count -> count = 1)
end

let solve input =
  let claims = In_channel.read_lines input |> List.map ~f:Claim.of_string in
  let map = claims |> List.fold ~init:Coords.Map.empty ~f:(fun map claim -> Claim.stake ~map ~claim) in
  claims 
  |> List.find_exn ~f:(fun claim -> Claim.safe ~map ~claim)

let () =
  let solution = solve "input" in
  Printf.printf "safe claim %i\n" solution.id
