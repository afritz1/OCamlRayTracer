open Constants;;
open Vec3;;

(* Camera definition. *)
type camera =
    { eye:vec3; forward:vec3; right:vec3; up:vec3; };;
    
(* Camera constructor. *)
let camera_make eye forward right up =
    { eye=eye; forward=forward; right=right; up=up; };;

(* Gets the aspect ratio of a camera. *)
let camera_get_aspect_ratio camera =
    vec3_length camera.right;;

(* Gets the zoom of a camera. *)
let camera_get_zoom camera =
    vec3_length camera.forward;;

(* Makes a camera looking at a point. *)
let camera_look_at eye focus aspect fov_y =
    let zoom = 1.0 /. (tan ((fov_y *. 0.50) *. deg_to_rad)) in
    let forward = vec3_scale (vec3_normalized (vec3_subtract focus eye)) zoom in
    let right = vec3_scale (vec3_normalized (vec3_cross forward vec3_unit_y)) aspect in
    let up = vec3_normalized (vec3_cross right forward) in
    camera_make eye forward right up;;

(* Gets an image direction based on the camera's directions and XY screen percentages. *)
let camera_image_direction camera xPercent yPercent =
    let right_comp = vec3_scale camera.right ((2.0 *. xPercent) -. 1.0)
    and up_comp = vec3_scale camera.up ((2.0 *. yPercent) -. 1.0) in
    vec3_normalized (vec3_add camera.forward (vec3_subtract right_comp up_comp))
