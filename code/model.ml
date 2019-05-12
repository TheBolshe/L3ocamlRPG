open Printf
open Tsdl
open Tsdl_image
open Result
open Sdl_tools
open Init_close


exception Invalid_team
exception Invalid_map_input
exception Invalid_land_type
exception Invalid_unit_input
exception Invalid_unit_type

let encode width i j =
  i + j * width

let decode width cell = (cell mod width, cell / width)

type direction = Up | Down | Left | Right
type team = Blue | Red
type typ = Archer | Warrior
type terrain = Land | Mountain | Sea | Road | Forest
type position = {lign : int; column : int}
type cursor = {cl : int; cc : int}
type cell = {lign : int; column : int; terrain : terrain}
type map = {height : int; width : int; tiles : cell list}
type unite = {position : position; typ : typ; team : team; hp : int; dmg : int}
type scene = {map : map; units : unite list; cursor : cursor; window : Sdl.window; renderer : Sdl.renderer}

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
      lign = lign;
      column = column;
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
      if column < width - 1 then
        create_cell (lign, column) (terrain_type x) :: tile_list lign (column + 1) s
      else
        create_cell (lign, column) (terrain_type x) :: tile_list (lign + 1) 0 s
  in
  {height = height; width = width; tiles = tile_list 0 0 tiles}

let create_scene map_string unit_string window renderer =
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
    units = (create_unit_list unit_info);
    cursor = {cl = 1; cc = 4};
    window = window;
    renderer = renderer
  }

let print_units liste =
  List.iter (fun x ->
      match x.typ with
      | Warrior -> printf "Warrior, HP : %i, DMG : %i\n" x.hp x.dmg
      | Archer -> printf "Archer, HP : %i, DMG : %i\n" x.hp x.dmg
    ) liste

let display_tile cell tile_height tile_width renderer =
  let display_color typ =
    match typ with
    | Land -> (255, 150, 0, 255)
    | Mountain -> (50, 25, 0, 255)
    | Sea -> (0, 0, 255, 255)
    | Road -> (150, 150, 150, 255)
    | Forest -> (0, 150, 0, 255)
  in
  match draw_filled_rectangle renderer (display_color cell.terrain) ((cell.lign * tile_height), ((cell.lign + 1) * tile_height), (cell.column * tile_width), ((cell.column + 1) * tile_width)) with
  | Error (`Msg e) -> Sdl.log "Failed draw tile : %s" e
  | Ok () -> ()

let display_tiles map window renderer =
  let (w_width, w_height) = Sdl.get_window_size window in
  let tile_width = w_width / map.width in
  let tile_height = w_height / map.height in
  List.iter (fun x -> display_tile x tile_width tile_height renderer) map.tiles

let display_unit unite unit_width unith_height renderer =
  ()

let display_cursor scene =
  let (w_width, w_height) = Sdl.get_window_size scene.window in
  let cursor_width = w_width / scene.map.width in
  let cursor_height = w_height / scene.map.height in
  match draw_filled_rectangle scene.renderer (255, 0, 0, 75) ((scene.cursor.cl * cursor_height), ((scene.cursor.cl + 1) * cursor_height), (scene.cursor.cc * cursor_width), ((scene.cursor.cc + 1) * cursor_width)) with
  | Error (`Msg e) -> Sdl.log "Failed draw cursor : %s" e
  | Ok () -> ()



let update_cursor direction scene =
  match direction with
  | Up -> {map = scene.map;
           units = scene.units;
           cursor = {
             cl = if scene.cursor.cl != 0 then scene.cursor.cl - 1 else scene.cursor.cl;
             cc = scene.cursor.cc
           };
           window = scene.window;
           renderer = scene.renderer}
  | Down ->  {map = scene.map;
              units = scene.units;
              cursor = {
                cl = if scene.cursor.cl != scene.map.height - 1 then scene.cursor.cl + 1 else scene.cursor.cl;
                cc = scene.cursor.cc
              };
              window = scene.window;
              renderer = scene.renderer}
  | Left ->  {map = scene.map;
              units = scene.units;
              cursor = {
                cl = scene.cursor.cl;
                cc = if scene.cursor.cc != 0 then scene.cursor.cc - 1 else scene.cursor.cc
              };
              window = scene.window;
              renderer = scene.renderer}
  | Right -> {map = scene.map;
              units = scene.units;
              cursor = {
                cl = scene.cursor.cl;
                cc = if scene.cursor.cc != scene.map.width - 1 then scene.cursor.cc + 1 else scene.cursor.cc
              };
              window = scene.window;
              renderer = scene.renderer}

let move_cursor_up scene =
  update_cursor Up scene

let move_cursor_down scene =
  update_cursor Down scene

let move_cursor_left scene =
  update_cursor Left scene

let move_cursor_right scene =
  update_cursor Right scene

let handle_event event scene =
  match Sdl.Event.enum (Sdl.Event.get event Sdl.Event.typ) with
  | `Window_event ->
    begin
      match Sdl.Event.window_event_enum (Sdl.Event.get event Sdl.Event.window_event_id) with
      | `Close -> shutdown scene.window scene.renderer
      | _ -> scene
    end
  | `Key_down ->
    if (Sdl.Event.get event Sdl.Event.keyboard_keycode) = Sdl.K.up then begin move_cursor_up scene end
    else if (Sdl.Event.get event Sdl.Event.keyboard_keycode) = Sdl.K.down then begin move_cursor_down scene end
    else if (Sdl.Event.get event Sdl.Event.keyboard_keycode) = Sdl.K.left then begin move_cursor_left scene end
    else if (Sdl.Event.get event Sdl.Event.keyboard_keycode) = Sdl.K.right then begin move_cursor_right scene end
    else if (Sdl.Event.get event Sdl.Event.keyboard_keycode) = Sdl.K.escape then begin shutdown scene.window scene.renderer end
    else scene
  | _ -> scene

let rec main_loop event scene =
  display_tiles scene.map scene.window scene.renderer;
  display_cursor scene;
  Sdl.render_present scene.renderer;
  begin
    match Sdl.poll_event (Some event) with
    | true -> main_loop event (handle_event event scene)
    | false -> main_loop event scene
  end



let () =
  let window = open_window in
  let renderer = create_renderer window in
  let event = Sdl.Event.create () in
  let str = "6 8 mlllsssslllfmsflsmrlflsmsllflrmssmrlrfllllsmrmss" in
  let unt = "1 1 w b 1 2 w b 2 1 w r 2 2 a r" in
  let scene = create_scene str unt window renderer in
  main_loop event scene
