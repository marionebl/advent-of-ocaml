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
end

let solve_one input =
  In_channel.read_lines input 
  |> List.map ~f:Claim.of_string
  |> List.fold ~init:Coords.Map.empty ~f:(fun map claim -> Claim.stake ~map ~claim)
  |> Coords.Map.count ~f:(fun v -> v > 1)

let () =
  let one = solve_one "input" in
  Printf.printf "overlapping %i\n" one