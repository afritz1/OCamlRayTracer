open Constants;;
open Intersection;;
open Material;;
open Ray;;
open Vec3;;

(* Sphere definition. *)
type sphere =
    { material:material; point:vec3; radius:float };;

(* Sphere constructor. *)
let sphere_make material point radius =
    { material=material; point=point; radius=radius };;

(* Sphere intersection function. The ray is typed because it was inferred as a sphere. *)
let sphere_intersect sphere (ray : ray) =
    let radius_recip = 1.0 /. sphere.radius
    and radius_squared = sphere.radius *. sphere.radius
    and diff = vec3_subtract sphere.point ray.point in
    let b = vec3_dot diff ray.direction in
    let determinant = (b *. b) -. (vec3_dot diff diff) +. radius_squared in
    if (determinant < 0.0) then None
    else
        let det_sqrt = sqrt determinant in
        let b1 = b -. det_sqrt
        and b2 = b +. det_sqrt in
        let t = 
            if (b1 > epsilon) then b1 
            else if (b2 > epsilon) then b2 
            else intersection_t_max in
        let point = ray_point_at ray t in
        let normal = vec3_scale (vec3_subtract point sphere.point) radius_recip in
        Some (intersection_make t point normal sphere.material);;
