open Vec3;;

(* Ray definition. *)
type ray = { point:vec3; direction:vec3; depth:int };;

let ray_default_depth = 0;;

(* Default ray constructor. *)
let ray_make_empty () = 
    let v = vec3_make_empty () in
    { point=v; direction=v; depth=ray_default_depth };;

(* Ray constructor with arguments. *)
let ray_make point direction depth =
    { point=point; direction=direction; depth=depth };;

(* Get a point at some distance along the ray. *)
let ray_point_at ray distance =
    vec3_add ray.point (vec3_scale ray.direction distance);;
