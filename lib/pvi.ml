type state = float * float
type state_derivative = state -> float -> state

let init_state f1 f2 = (f1, f2)
let add_state (a0, a1) (b0, b1) = (a0 +. b0, a1 +. b1)
let scale_state t (a0, a1) = (t *. a0, t *. a1)

let derivative_of_state expr state_ t1 =
  let v0, _ = state_ in
  (expr state_ t1, v0)

let init_derivative expr = derivative_of_state expr

let runge_kutta state_ t_i derivative dt =
  let f1 = derivative state_ t_i in
  let half_dt = dt /. 2. in
  let s_one_half = add_state state_ (scale_state half_dt f1) in
  let f2 = derivative s_one_half (t_i +. half_dt) in

  let minus_f1 = scale_state (-1.) f1 in
  let double_one_half = scale_state 2. s_one_half in
  let s_approx =
    add_state state_ (scale_state dt (add_state minus_f1 double_one_half))
  in
  let f3 = derivative s_approx (t_i +. dt) in

  let f1_aux = scale_state (1. /. 6.) f1
  and f2_aux = scale_state (4. /. 6.) f2
  and f3_aux = scale_state (1. /. 6.) f3 in

  let aux_sum = add_state f1_aux (add_state f2_aux f3_aux) in

  add_state state_ (scale_state dt aux_sum)

let get_nth_state initial_state t_i derivative dt last_t =
  let rec aux iter t_aux rslt =
    if iter = 0 then rslt
    else aux (iter - 1) (t_aux +. dt) (runge_kutta rslt t_aux derivative dt)
  in
  let iterations = int_of_float ((last_t -. t_i) /. dt) in
  aux iterations t_i initial_state

let plot_square x y sq_size =
  Graphics.fill_rect (x - sq_size) (y - sq_size) (sq_size * 2) (sq_size * 2)

let plot_function initial_state t_i derivative dt last_t =
  Graphics.open_graph "";
  let size_x = Graphics.size_x () and size_y = Graphics.size_y () in
  let x_center, y_center = (size_x / 2, size_y / 2) in
  (* Draw the Y line *)
  Graphics.moveto x_center 0;
  Graphics.lineto x_center size_y;
  (* Draw the X line *)
  Graphics.moveto 0 y_center;
  Graphics.lineto size_x y_center;

  let adjust_time t scale_factor =
    int_of_float (t +. float_of_int x_center +. (t *. scale_factor))
  and adjust_position y = int_of_float (y +. float_of_int y_center) in
  for time = int_of_float t_i to int_of_float last_t do
    let f_time = float_of_int time in
    let _, y0 = get_nth_state initial_state t_i derivative dt f_time in
    Printf.printf "[DEBUG]: TIME: %f; POSITION: %f\n" f_time y0;
    let new_time = adjust_time f_time 20.
    and new_position = adjust_position y0 in
    plot_square new_time new_position 3
  done;

  let _ = Graphics.read_key () in
  ()
