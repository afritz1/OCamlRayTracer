open Constants;;
open Intersection;;
open Material;;
open Ray;;
open Sphere;;
open Vec3;;

(* World definition. *)
type world =
    { spheres:sphere list; background:vec3 };;

(* World default constructor. *)
let world_make_empty () =
    { spheres=[]; background=vec3_make_empty () };;

(* World constructor. *)
let world_make spheres background =
    { spheres=spheres; background=background };;

(* World random constructor. *)
let world_make_random sphere_count world_radius =
    let make_sphere world_radius =
        let material = material_make_random ()
        and point = vec3_rand_sphere_point world_radius
        and radius = 0.5 +. (Random.float 1.0) in
        sphere_make material point radius in
    let make_spheres sphere_count = 
        let sphere_list = ref [] in
        for i = 1 to sphere_count do
            sphere_list := List.append !sphere_list [make_sphere world_radius];
        done;
        !sphere_list in
    world_make (make_spheres sphere_count) (vec3_rand_color ());;

(* Gets the nearest hit shape (or none) by a ray. *)
let world_nearest_hit world ray =
    let nearest_hit : intersection option ref = ref None in
    let test_sphere sphere ray =
        let current_try = sphere_intersect sphere ray in
        match current_try with
            | Some new_hit -> 
                (match !nearest_hit with
                | Some old_hit -> (if new_hit.t < old_hit.t then nearest_hit := current_try)
                | None -> nearest_hit := current_try);
            | None -> () in
    List.iter (fun s -> test_sphere s ray) world.spheres;
    !nearest_hit;;

(* Gets the ambient occlusion percent at a point. *)
let world_get_ambient_percent world point normal =
    let point_normal_epsilon = vec3_add point (vec3_scale normal epsilon)
    and visible_samples = ref 0
    and max_samples = 256 in
    for n = 1 to max_samples do
        let hemisphere_dir = vec3_rand_hemisphere_direction normal in
        let hemisphere_ray = ray_make point_normal_epsilon 
            hemisphere_dir ray_default_depth in
        let current_try = world_nearest_hit world hemisphere_ray in
        match current_try with
        | Some hit -> (if (hit.t = intersection_t_max) then 
            visible_samples := !visible_samples + 1);
        | None -> visible_samples := !visible_samples + 1;
    done;
    (float_of_int !visible_samples) /. (float_of_int max_samples);;

(* Gets the shaded color of a material. *)
let world_get_shaded_color world t point normal material =
    let ambient = world_get_ambient_percent world point normal in
    vec3_scale material.color ambient;;

(* Gets the color of a traced ray. *)
let world_ray_trace world ray =
    let nearest_hit = world_nearest_hit world ray in
    match nearest_hit with
        | Some hit -> (world_get_shaded_color world hit.t hit.point hit.normal hit.material)
        | None -> world.background;;
        