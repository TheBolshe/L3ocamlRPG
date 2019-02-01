open Tsdl
open Result

let open_window =
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error : %s" e; exit 1
  | Ok () ->
    match Sdl.create_window "test" ~w:640 ~h:480 Sdl.Window.opengl with
    | Error (`Msg e) -> Sdl.log "Window opening error : %s" e; Sdl.quit; exit 1
    | Ok w -> w

let () =
  let window = open_window in
  Sdl.delay 1000l;
  Sdl.destroy_window window;
  Sdl.quit;
  exit 1
