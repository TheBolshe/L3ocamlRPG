open Printf

type terrain = {width : int; height : int; tiles : string list}

let map1 = {width = 6; height = 4; tiles = ["s"; "s"; "l"; "m"; "m"; "l";
                                            "s"; "s"; "l"; "l"; "l"; "l";
                                            "s"; "l"; "l"; "m"; "l"; "l";
                                            "l"; "l"; "m"; "m"; "l"; "l"]}

type 'a objets =
    Vide
  | Noeud of 'a * 'a objets list

let encode width i j =
  i + j * width

let decode width cell =
  ((cell mod width), cell / width)


type team = Blue | Red
type typ = Archer | Warrior
type position = {lign : int; column : int}
type unite = {position : position; typ : typ; team : team; hp : int; dmg : int}

let create_unit (lign, column) typ team =
  match typ with
  | Archer -> {position = {lign = lign; column = column}; typ = Archer; team = team; hp = 10; dmg = 5}
  | Warrior -> {position = {lign = lign; column = column}; typ = Warrior; team = team; hp = 15; dmg = 5}

let print_units liste =
  List.iter (fun x ->
      match x.typ with
      | Warrior -> printf "Warrior, HP : %i, DMG : %i\n" x.hp x.dmg
      | Archer -> printf "Archer, HP : %i, DMG : %i\n" x.hp x.dmg
    ) liste

let () =
  let a = create_unit (1, 2) Warrior Blue in
  let b = create_unit (2, 2) Warrior Blue in
  let c = create_unit (3, 2) Warrior Blue in
  let d = create_unit (2, 1) Archer Blue in
  let liste = [a; b; c; d] in
  print_units liste
