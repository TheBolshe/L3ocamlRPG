open Tsdl
open Tsdl_image
open Result
open Sdl_tools
open Init_close
open Model

let display renderer =
  match load_image renderer "../data/pictures/fish.jpg" with
  | Error (`Msg e) -> Sdl.log "Failed create texture : %s" e
  | Ok tex -> Sdl.render_clear renderer;
    render_texture renderer (0,480,0,640) tex Sdl.Flip.none;
    Sdl.render_present renderer;
    match load_image renderer "../data/pictures/fish.bmp" with
    | Error (`Msg e) -> Sdl.log "Failed create texture : %s" e
    | Ok tex -> render_texture renderer (0,240,0,320) tex Sdl.Flip.none;
      Sdl.render_present renderer

let rec main_loop quit event window renderer =
  display renderer;
  match quit with
  | false ->
    begin
      match Sdl.poll_event (Some event) with
      | true -> main_loop (Events.handle_event event) event window renderer
      | false -> main_loop false event window renderer
    end
  | true -> shutdown window renderer


let () =
  let window = open_window in
  let renderer = create_renderer window in
  let event = Sdl.Event.create () in
  main_loop false event window renderer
