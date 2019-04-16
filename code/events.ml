open Tsdl

let handle_event event =
  match Sdl.Event.enum (Sdl.Event.get event Sdl.Event.typ) with
  | `Window_event ->
    begin
      match Sdl.Event.window_event_enum (Sdl.Event.get event Sdl.Event.window_event_id) with
      | `Close ->  true
      | _ -> false
    end
  | `Key_down -> true
  | _ -> false
