open Vec3;;

(* Material definition. *)
type material =
    { color:vec3 };;

(* Material constructor. *)
let material_make color =
    { color=color };;

(* Material random constructor. *)
let material_make_random () = 
    { color=vec3_rand_color () };;
