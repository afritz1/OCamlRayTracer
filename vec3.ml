open Constants;;

(* vec3 definition. *)
type vec3 = { x:float; y:float; z:float };;

(* Default vec3 constructor. *)
let vec3_make_empty () = { x=0.0; y=0.0; z=0.0; };;

(* vec3 constructor with arguments. *)
let vec3_make x y z = { x=x; y=y; z=z; };;

(* Unit vectors. *)
let vec3_unit_x = { x=1.0; y=0.0; z=0.0 };;
let vec3_unit_y = { x=0.0; y=1.0; z=0.0 };;
let vec3_unit_z = { x=0.0; y=0.0; z=1.0 };;

(* Length squared of a vec3. *)
let vec3_length_squared v =
    (v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z);;

(* Length of a vec3. *)
let vec3_length v =
    sqrt (vec3_length_squared v);;

(* Returns a normalized vec3. *)
let vec3_normalized v =
    let len_recip = 1.0 /. (vec3_length v) in
    vec3_make (v.x *. len_recip) (v.y *. len_recip) (v.z *. len_recip);;

(* Dot product of two vec3's. *)
let vec3_dot v1 v2 =
    (v1.x *. v2.x) +. (v1.y *. v2.y) +. (v1.z *. v2.z);;

(* Cross product of two vec3's. *)
let vec3_cross v1 v2 =
    let x = (v1.y *. v2.z) -. (v2.y *. v1.z)
    and y = (v2.x *. v1.z) -. (v1.x *. v2.z)
    and z = (v1.x *. v2.y) -. (v2.x *. v1.y) in
    vec3_make x y z;;

(* Sum of two vec3's. *)
let vec3_add v1 v2 =
    vec3_make (v1.x +. v2.x) (v1.y +. v2.y) (v1.z +. v2.z);;

(* Difference of two vec3's. *)
let vec3_subtract v1 v2 =
    vec3_make (v1.x -. v2.x) (v1.y -. v2.y) (v1.z -. v2.z);;

(* Negation of a vec3. *)
let vec3_negate v =
    vec3_make (-.v.x) (-.v.y) (-.v.z);;

(* Scales a vec3 by a multiplier. *)
let vec3_scale v m =
    vec3_make (v.x *. m) (v.y *. m) (v.z *. m);;

(* Multiplies a vec3 by another vec3. *)
let vec3_multiply v1 v2 =
    vec3_make (v1.x *. v2.x) (v1.y *. v2.y) (v1.z *. v2.z);;

(* Divides a vec3 by another vec3. *)
let vec3_divide v1 v2 =
    vec3_make (v1.x /. v2.x) (v1.y /. v2.y) (v1.z /. v2.z);;

(* Reflects a vec3 around another vec3. *)
let vec3_reflect v1 v2 =
    let vn_dot = vec3_dot v1 v2 in
    let vn_dot2 = vn_dot *. 2.0
    and vnSign = if (vn_dot > 0.0) then 1.0 else (if (vn_dot < 0.0) then -1.0 else 0.0) in
    vec3_make (((vnSign *. v2.x) *. vn_dot2) -. v1.x)
        (((vnSign *. v2.y) *. vn_dot2) -. v1.y)
        (((vnSign *. v2.z) *. vn_dot2) -. v1.z);;

(* Linearly interpolates a vec3 with another vec3. *)
let vec3_lerp v1 v2 percent =
    vec3_make (v1.x +. ((v2.x -. v1.x) *. percent))
        (v1.y +. ((v2.y -. v1.y) *. percent))
        (v1.z +. ((v2.z -. v1.z) *. percent));;

(* Clamps a vec3 between 0.0 and 1.0. *)
let vec3_clamped v =
    let low = 0.0
    and high = 1.0 in
    vec3_make (if (v.x > high) then high else (if (v.x < low) then low else v.x))
        (if (v.y > high) then high else (if (v.y < low) then low else v.y))
        (if (v.z > high) then high else (if (v.z < low) then low else v.z));;

(* Generates a random RGB 0->1 color. *)
let vec3_rand_color () =
    vec3_make (Random.float 1.0) (Random.float 1.0) (Random.float 1.0);;

(* Gets a random point within a sphere at the origin. *)
let vec3_rand_sphere_point radius =
    let rand_point = vec3_normalized (vec3_make ((Random.float 1.0) -. 0.50)
        ((Random.float 1.0) -. 0.50)
        ((Random.float 1.0) -. 0.50)) in
    vec3_scale rand_point (radius *. (Random.float 1.0));;

(* Gets a random point within a cuboid at the origin. *)
let vec3_rand_cuboid_point width height depth =
    let rand_point = vec3_make (width *. ((Random.float 1.0) -. 0.50))
        (height *. ((Random.float 1.0) -. 0.50))
        (depth *. ((Random.float 1.0) -. 0.50)) in
    rand_point;;

let vec3_rand_hemisphere_direction normal =
    let rand_dir = vec3_normalized (vec3_make ((Random.float 1.0) -. 0.50)
        ((Random.float 1.0) -. 0.50)
        ((Random.float 1.0) -. 0.50)) in
    if (vec3_dot rand_dir normal) >= 0.0 then rand_dir else (vec3_negate rand_dir);;

(* Initialize the random number generator seed. *)
let () =
    Random.self_init ();;
