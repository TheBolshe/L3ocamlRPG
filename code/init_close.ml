open Result
open Tsdl

let open_window =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Failed to init : %s" e; exit 1
  | Ok () ->
    match Sdl.create_window "test" ~w:640 ~h:480 Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Failed to open window : %s" e; Sdl.quit (); exit 1
    | Ok w -> w

let create_renderer w =
  match Sdl.create_renderer ~index:(-1) ~flags:Sdl.Renderer.(presentvsync + accelerated) w with
  | Error (`Msg e) -> Sdl.log "Failed create renderer : %s" e; Sdl.destroy_window w; Sdl.quit (); exit 1
  | Ok r -> r

let shutdown window renderer=
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window window;
  Sdl.quit ();
  exit 1
