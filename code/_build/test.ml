open Tsdl
open Tsdl_image
open Result
open Sdl_tools

let open_window =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error : %s" e; exit 1
  | Ok () ->
    match Sdl.create_window "test" ~w:640 ~h:480 Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Window opening error : %s" e; Sdl.quit (); exit 1
    | Ok w -> w

let create_renderer w =
  match Sdl.create_renderer ~index:(-1) ~flags:Sdl.Renderer.(presentvsync + accelerated) w with
  | Error (`Msg e) -> Sdl.log "Failed create renderer : %s" e; Sdl.destroy_window w; Sdl.quit (); exit 1
  | Ok r -> r

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

let () =
  let window = open_window in
  let renderer = create_renderer window in
  let event = Sdl.Event.create () in
  Sdl.delay 1000l;
  Sdl.destroy_window window;
  Sdl.quit ();
  exit 1


let rec loop renderer event =
  if Sdl.poll_event event 
