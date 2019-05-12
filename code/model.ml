open Printf


exception Invalid_team
exception Invalid_map_input
exception Invalid_land_type
exception Invalid_unit_input
exception Invalid_unit_type

let encode width i j =
  i + j * width

let decode width cell = (cell mod width, cell / width)


type team = Blue | Red
type typ = Archer | Warrior
type terrain = Land | Mountain | Sea | Road | Forest
type position = {lign : int; column : int}
type cell = {position : position; terrain : terrain}
type map = {height : int; width : int; tiles : cell list}
type unite = {position : position; typ : typ; team : team; hp : int; dmg : int}
type scene = {map : map; units : unite list}

let create_unit (lign, column) typ team =
  let team_conv t =
    match t with
    | "b" -> Blue
    | "r" -> Red
    | _ -> (raise Invalid_team)
  in
  match typ with
  | "a" -> {position = {lign = lign; column = column}; typ = Archer; team = (team_conv team); hp = 10; dmg = 5}
  | "w" -> {position = {lign = lign; column = column}; typ = Warrior; team = (team_conv team); hp = 15; dmg = 5}
  | _ -> (raise Invalid_unit_type)

let rec create_unit_list string_info =
  match string_info with
  | [] -> []
  | lign :: column :: typ :: team :: s -> (create_unit (int_of_string lign, int_of_string column) typ team) :: create_unit_list s
  | _ -> (raise Invalid_unit_input)

let create_map height width tiles =
  let create_cell (lign, column) terrain =
    {
      position = {
        lign = lign;
        column = column
      };
      terrain = terrain
    }
  in
  let terrain_type s =
    match s with
    | 'l' -> Land
    | 'm' -> Mountain
    | 's' -> Sea
    | 'r' -> Road
    | 'f' -> Forest
    | _ -> (raise Invalid_land_type)
  in
  let rec tile_list lign column tiles =
    match tiles with
    | [] -> []
    | x :: s ->
      if column < width then
        create_cell (lign, column) (terrain_type x) :: tile_list lign (column + 1) s
      else
        create_cell (lign, column) (terrain_type x) :: tile_list (lign + 1) 0 s
  in
  {height = height; width = width; tiles = tile_list 0 0 tiles}

let create_scene map_string unit_string =
  let explode s =
    let rec expl i l =
      if i < 0 then l else
        expl (i - 1) (s.[i] :: l) in
    expl (String.length s - 1) []
  in
  let map_info = String.split_on_char ' ' map_string in
  let unit_info = String.split_on_char ' ' unit_string in
  {
    map = (match map_info with
        | height :: width :: tiles -> create_map (int_of_string height) (int_of_string width) (explode (List.hd tiles))
        | _ -> (raise Invalid_map_input));
    units = (create_unit_list unit_info)
  }


let print_units liste =
  List.iter (fun x ->
      match x.typ with
      | Warrior -> printf "Warrior, HP : %i, DMG : %i\n" x.hp x.dmg
      | Archer -> printf "Archer, HP : %i, DMG : %i\n" x.hp x.dmg
    ) liste

let () =
  let str = "3 4 llllllllllll" in
  let unt = "1 1 w b 1 2 w b 2 1 w r 2 2 a r" in
  let scene = create_scene str unt in
  print_units scene.units
