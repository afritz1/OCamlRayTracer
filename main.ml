open Camera;;
open Ray;;
open Vec3;;
open World;;

let screen_width = 1280;;
let screen_height = 720;;
let aspect = (float_of_int screen_width) /. (float_of_int screen_height);;
let fov_y = 60.0;;

let sphere_count = 100;;
let world_radius = 16.0;;

let out_filename = "output.ppm";;

let () =
    let camera = 
        let eye = vec3_make 0.0 0.0 32.0 
        and focus = vec3_make 0.0 0.0 0.0 in
        camera_look_at eye focus aspect fov_y in
    let world = world_make_random sphere_count world_radius in
    let colors = Array.make (screen_width * screen_height) (vec3_make_empty ()) in
    
    (* Trace rays. *)
    for y = 0 to (screen_height - 1) do
        let yy = (float_of_int y) /. (float_of_int screen_height) in
        for x = 0 to (screen_width - 1) do
            let index = x + (y * screen_width) in
            let xx = (float_of_int x) /. (float_of_int screen_width) in
            let ray_direction = camera_image_direction camera xx yy in
            let ray = ray_make camera.eye ray_direction ray_default_depth in
            let color = world_ray_trace world ray in
            colors.(index) <- vec3_clamped color;
        done;
    done;
    
    (* Prepare PPM file. *)
    let out_file = open_out out_filename in
    let comment = "# OCaml Ray Tracer output image." in
    Printf.fprintf out_file "P3\n%s\n%d %d\n%d\n" comment screen_width screen_height 255;
    
    (* Write colors to the file. *)
    for y = 0 to (screen_height - 1) do
        for x = 0 to (screen_width - 1) do
            let index = x + (y * screen_width) in
            let color = colors.(index) in
            let r = int_of_float (color.x *. 255.0)
            and g = int_of_float (color.y *. 255.0)
            and b = int_of_float (color.z *. 255.0) in
            Printf.fprintf out_file "%d %d %d " r g b;
        done;
        Printf.fprintf out_file "\n";
    done;
    close_out out_file;;
