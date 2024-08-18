type state = float * float
type state_derivative = state -> float -> state

let init_state f1 f2 = (f1, f2)
let add_state (a0, a1) (b0, b1) = (a0 +. b0, a1 +. b1)
let scale_state t (a0, a1) = (t *. a0, t *. a1)

let third_runge_kutta state_ t_i derivative dt =
  let f1 = derivative state_ t_i in
  let half_dt = dt /. 2. in
  let s_one_half = add_state state_ (scale_state half_dt f1) in
  let f2 = derivative s_one_half (t_i +. half_dt) in

  let minus_f1 = scale_state (-1.) f1 in
  let double_f2 = scale_state 2. f2 in
  let s_approx =
    add_state state_ (scale_state dt (add_state minus_f1 double_f2))
  in
  let f3 = derivative s_approx (t_i +. dt) in

  let f1_aux = scale_state (1. /. 6.) f1
  and f2_aux = scale_state (4. /. 6.) f2
  and f3_aux = scale_state (1. /. 6.) f3 in

  let aux_sum = add_state f1_aux (add_state f2_aux f3_aux) in

  add_state state_ (scale_state dt aux_sum)

let fourth_runge_kutta state_ t_i derivative dt =
  let f1 = derivative state_ t_i in
  let half_dt = dt /. 2. in
  let s2 = add_state state_ (scale_state half_dt f1) in
  let f2 = derivative s2 (t_i +. half_dt) in
  let s3 = add_state state_ (scale_state half_dt f2) in
  let f3 = derivative s3 (t_i +. half_dt) in
  let s4 = add_state state_ (scale_state dt f3) in
  let f4 = derivative s4 (t_i +. dt) in

  let aux_sum =
    add_state
      (add_state (add_state f1 (scale_state 2. f2)) (scale_state 2. f3))
      f4
  in

  add_state state_ (scale_state (dt /. 6.) aux_sum)

let is_close_enough (c1, c2) (p1, p2) eps =
  let ep1 = (c1 -. p1) /. c1 and ep2 = (c2 -. p2) /. c2 in
  ep2 <= eps && ep1 <= eps

let predictor_corrector s0 s1 s2 s3 t_i derivative dt eps =
  let f0 = derivative s0 t_i
  and f1 = derivative s1 (t_i +. dt)
  and f2 = derivative s2 (t_i +. (2. *. dt))
  and f3 = derivative s3 (t_i +. (3. *. dt)) in

  let aux_sum =
    let aux_1 = scale_state (-9.) f0
    and aux_2 = scale_state 37. f1
    and aux_3 = scale_state (-59.) f2
    and aux_4 = scale_state 55. f3 in
    add_state aux_1 (add_state aux_2 (add_state aux_3 aux_4))
  in
  let approx_s4 = add_state s3 (scale_state (dt /. 24.) aux_sum) in

  let rec get_accurate_s1 previous_s4 =
    let previous_f4 = derivative previous_s4 (t_i +. (4. *. dt)) in
    let aux_sum =
      let aux_f2 = scale_state (-5.) f2
      and aux_f3 = scale_state 19. f3
      and aux_acc = scale_state 9. previous_f4 in
      add_state f1 (add_state aux_f2 (add_state aux_f3 aux_acc))
    in
    let current_s4 = add_state s3 (scale_state (dt /. 24.) aux_sum) in
    if is_close_enough current_s4 previous_s4 eps then current_s4
    else get_accurate_s1 current_s4
  in

  get_accurate_s1 approx_s4

let plot_square x y sq_size =
  Graphics.fill_rect (x - sq_size) (y - sq_size) (sq_size * 2) (sq_size * 2)

let plot_function_fst initial_state t_i derivative dt final_t =
  Graphics.open_graph "";
  let size_x = Graphics.size_x () and size_y = Graphics.size_y () in
  let x_center, y_center = (size_x / 8, size_y / 2) in
  (* Draw the Y line *)
  Graphics.moveto x_center 0;
  Graphics.lineto x_center size_y;
  (* Draw the X line *)
  Graphics.moveto 0 y_center;
  Graphics.lineto size_x y_center;

  let adjust_time t scale_factor =
    int_of_float (t +. float_of_int x_center +. (t *. scale_factor))
  and adjust_position y = int_of_float (y +. float_of_int y_center) in

  let rec plot_loop current_time ((_, current_position) as current_state) =
    if current_time > final_t then ()
    else
      let new_time = adjust_time current_time 40.
      and new_position = adjust_position current_position in
      Printf.printf "[DEBUG]: TIME: %f; POSITION: %f\n" current_time
        current_position;
      plot_square new_time new_position 2;
      let new_state =
        third_runge_kutta current_state current_time derivative dt
      in
      plot_loop (current_time +. dt) new_state
  in
  plot_loop t_i initial_state;
  let _ = Graphics.read_key () in
  ()

let plot_function_snd initial_state t_i derivative dt final_t =
  Graphics.open_graph "";
  let size_x = Graphics.size_x () and size_y = Graphics.size_y () in
  let x_center, y_center = (size_x / 8, size_y / 2) in
  (* Draw the Y line *)
  Graphics.moveto x_center 0;
  Graphics.lineto x_center size_y;
  (* Draw the X line *)
  Graphics.moveto 0 y_center;
  Graphics.lineto size_x y_center;

  let adjust_time t scale_factor =
    int_of_float (t +. float_of_int x_center +. (t *. scale_factor))
  and adjust_speed y = int_of_float (y +. float_of_int y_center) in

  let rec plot_loop current_time ((current_speed, _) as current_state) =
    if current_time > final_t then ()
    else
      let new_time = adjust_time current_time 40.
      and new_speed = adjust_speed current_speed in
      Printf.printf "[DEBUG]: TIME: %f; SPEED: %f\n" current_time current_speed;
      plot_square new_time new_speed 2;
      let new_state =
        third_runge_kutta current_state current_time derivative dt
      in
      plot_loop (current_time +. dt) new_state
  in
  plot_loop t_i initial_state;
  let _ = Graphics.read_key () in
  ()

let plot_function_fst_PC initial_state t_i derivative dt final_t eps =
  Graphics.open_graph "";
  let size_x = Graphics.size_x () and size_y = Graphics.size_y () in
  let x_center, y_center = (size_x / 8, size_y / 2) in
  (* Draw the Y line *)
  Graphics.moveto x_center 0;
  Graphics.lineto x_center size_y;
  (* Draw the X line *)
  Graphics.moveto 0 y_center;
  Graphics.lineto size_x y_center;

  let adjust_time t scale_factor =
    int_of_float (t +. float_of_int x_center +. (t *. scale_factor))
  and adjust_position y = int_of_float (y +. float_of_int y_center) in

  let _, p1 = initial_state in

  Printf.printf "[DEBUG]: TIME: %f; POSITION: %f\n" t_i p1;
  let t_i = t_i +. dt in

  let ((_, p1) as s1) = fourth_runge_kutta initial_state t_i derivative dt in
  plot_square (adjust_time t_i 40.) (adjust_position p1) 2;
  Printf.printf "[DEBUG]: TIME: %f; POSITION: %f\n" t_i p1;
  let t_i = t_i +. dt in

  let ((_, p2) as s2) = fourth_runge_kutta s1 t_i derivative dt in
  plot_square (adjust_time t_i 40.) (adjust_position p2) 2;
  Printf.printf "[DEBUG]: TIME: %f; POSITION: %f\n" t_i p2;
  let t_i = t_i +. dt in

  let s3 = fourth_runge_kutta s2 t_i derivative dt in

  let rec plot_loop current_time state_0 state_1 state_2
      ((_, current_position) as state_3) =
    if current_time > final_t then ()
    else
      let new_time = adjust_time current_time 40.
      and new_position = adjust_position current_position in
      Printf.printf "[DEBUG]: TIME: %f; POSITION: %f\n" current_time
        current_position;
      plot_square new_time new_position 2;
      let new_state =
        predictor_corrector state_0 state_1 state_2 state_3 current_time
          derivative dt eps
      in
      plot_loop (current_time +. dt) state_1 state_2 state_3 new_state
  in
  plot_loop t_i initial_state s1 s2 s3;
  let _ = Graphics.read_key () in
  ()

let plot_function_snd_PC initial_state t_i derivative dt final_t eps =
  Graphics.open_graph "";
  let size_x = Graphics.size_x () and size_y = Graphics.size_y () in
  let x_center, y_center = (size_x / 8, size_y / 2) in
  (* Draw the Y line *)
  Graphics.moveto x_center 0;
  Graphics.lineto x_center size_y;
  (* Draw the X line *)
  Graphics.moveto 0 y_center;
  Graphics.lineto size_x y_center;

  let adjust_time t scale_factor =
    int_of_float (t +. float_of_int x_center +. (t *. scale_factor))
  and adjust_speed y = int_of_float (y +. float_of_int y_center) in

  let p1, _ = initial_state in

  Printf.printf "[DEBUG]: TIME: %f; SPEED: %f\n" t_i p1;
  let t_i = t_i +. dt in

  let ((p1, _) as s1) = fourth_runge_kutta initial_state t_i derivative dt in
  plot_square (adjust_time t_i 40.) (adjust_speed p1) 2;
  Printf.printf "[DEBUG]: TIME: %f; SPEED: %f\n" t_i p1;
  let t_i = t_i +. dt in

  let ((p2, _) as s2) = fourth_runge_kutta s1 t_i derivative dt in
  plot_square (adjust_time t_i 40.) (adjust_speed p2) 2;
  Printf.printf "[DEBUG]: TIME: %f; SPEED: %f\n" t_i p2;
  let t_i = t_i +. dt in

  let s3 = fourth_runge_kutta s2 t_i derivative dt in

  let rec plot_loop current_time state_0 state_1 state_2
      ((current_speed, _) as state_3) =
    if current_time > final_t then ()
    else
      let new_time = adjust_time current_time 40.
      and new_position = adjust_speed current_speed in
      Printf.printf "[DEBUG]: TIME: %f; SPEED: %f\n" current_time current_speed;
      plot_square new_time new_position 2;
      let new_state =
        predictor_corrector state_0 state_1 state_2 state_3 current_time
          derivative dt eps
      in
      plot_loop (current_time +. dt) state_1 state_2 state_3 new_state
  in
  plot_loop t_i initial_state s1 s2 s3;
  let _ = Graphics.read_key () in
  ()
