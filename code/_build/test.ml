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


let () =
  let window = open_window in
  let renderer = create_renderer window in
  match load_image renderer "../data/pictures/fish.jpg" with
  | Error (`Msg e) -> Sdl.log "Failed create texture : %s" e
  | Ok tex -> tex;
    Sdl.render_clear renderer;
    let a = render_texture renderer (0,480,0,640) tex Sdl.Flip.none in
    let a = Sdl.render_present renderer in
    Sdl.delay 1000l;
    match load_image renderer "../data/pictures/fish.bmp" with
    | Error (`Msg e) -> Sdl.log "Failed create texture : %s" e
    | Ok tex -> tex;
      render_texture renderer (0,240,0,320) tex Sdl.Flip.none;
      Sdl.render_present renderer;
      Sdl.delay 1000l;
      Sdl.destroy_window window;
      Sdl.quit ();
      exit 1
