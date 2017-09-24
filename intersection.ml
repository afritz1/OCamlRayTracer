open Material;;
open Vec3;;

(* Intersection definition. *)
type intersection =
    { t:float; point:vec3; normal:vec3; material:material };;

let intersection_t_max = infinity;;

(* Default intersection constructor. *)
let intersection_make_empty () =
    let v = vec3_make_empty() in
    { t=intersection_t_max; point=v; normal=v; material=material_make_random () };;

(* Intersection constructor. *)
let intersection_make t point normal material =
    { t=t; point=point; normal=normal; material=material };;
