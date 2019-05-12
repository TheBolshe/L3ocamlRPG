(*
let print_map map =
  let tiles = map.tiles in
  let show tiles =
    match tiles with
    | [] -> ()
    | head -> ()
    | head :: tail -> ()

let rec print_unit liste =
  match liste with
  | [] -> ()
  | x ->
    begin
      match x.typ with
      | Warrior -> Printf.printf "Warrior, HP : %i, DMG : %i\n" x.hp x.dmg
      | Archer -> Printf.printf "Archer, HP : %i, DMG : %i\n" x.hp x.dmg
    end
  | x :: s ->
    begin
      print_unit x;
      print_unit s
    end
*)
