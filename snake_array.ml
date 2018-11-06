#load "graphics.cma";;
open Graphics;;

(*type snake*)
type snake ={mutable len : int;mutable hx:int;mutable hy:int;mutable ty:int;mutable tx:int;mutable cur:(int*int) array;mutable col: color; mutable dir: char; mutable alive:bool  };;

(*type food *)
type food ={mutable posx : int ; mutable posy : int};;

(*show food on graphics*)
let show_food food =
set_color black;
fill_circle (food.posx) (food.posy) 2;;

(*snake on graph*)
let show_snake snake =
	set_color snake.col;
set_line_width 5;
 draw_poly_line (snake.cur);
 set_line_width 1;;

(*x co-ordinate of point*)
let ptx a =
 match a with
 | x,y -> x;;

(*y coo-ordinate of point*)
 let pty a =
 match a with
 | x,y -> y;;


(*increase len of x*)
let inc_snake snake n = 
snake.len <- snake.len + n;
for i = 0 to 0 do
match snake.dir with
| '6' ->begin snake.hx <- snake.hx + n ; end 
| '2' ->begin snake.hy <- snake.hy - n ; end
| '4' ->begin snake.hx <- snake.hx - n ; end
| '8' ->begin snake.hy <- snake.hy + n ; end
| _ ->begin (); end
done;
let l = snake.cur in
if (snake.hx = ptx (l.(0)) && ptx (l.(0)) = ptx (l.(1))) || (snake.hy = pty (l.(0)) && pty (l.(0)) = pty (l.(1))) then
	begin l.(0) <- snake.hx,snake.hy; end
else begin
snake.cur <-Array.append [|snake.hx,snake.hy|] (snake.cur) end;;

 (*distance between two points of snake*)
 let dis2 l n =
 match l.(n),l.(n-1) with
 |(a,b),(c,d) -> (abs (a-c)) + (abs (b-d));;

(*short form for float_of_int*) 
let f = float_of_int;;

(*distance between two horizontal or verticle points*)
let dis a b =
	match a,b with
	| (x1,y1),(x2,y2) -> (((f (x1 - x2))**2.0 +. (f (y1 - y2))**2.0)**0.5);; 

(*checks if point x , y is on snake or not*)
let check_point snake x y m=
 let l = snake.cur in
 let i = ref 0 in
 let n = Array.length l - 1 in
 let k = ref true in
 if m = 0 then
               begin
               while (!i < n) && !k do
               k := not ( dis (l.(!i)) (l.(!i+1)) = (dis (l.(!i)) (x,y)) +. (dis (x,y) (l.(!i+1))));
               i := !i + 1; done;
               !k 
               end
 else 
               begin 
               i := 1;
               while (!i < n) && !k do
               k := not ( dis (l.(!i)) (l.(!i+1)) = (dis (l.(!i)) (x,y)) +. (dis (x,y) (l.(!i+1))));
               i := !i + 1; done;
               !k 
  end ;;

let eat_food snake food s q=
	if snake.hx = food.posx  && snake.hy = food.posy  then
    begin inc_snake snake 5;
    let nx = ref snake.hx in
    let ny = ref snake.hy in
    while (check_point snake (!nx) (!ny) 0) do
    nx := (((Random.int (size_x()-5))/5)*5 + 5);
    ny := (((Random.int (size_y()-5))/5)*5 + 5); done;
    food.posx <- !nx;
    food.posy <- !ny; 
    q := !q -. 0.005;
    sound 500 2; end
else ();;

(*returns array with last element eliminated*)
let rem l =
	let n = Array.length l - 1 in
	let c = Array.make (n) (0,0) in
	for i = 0 to n - 1 do
	c.(i) <- l.(i) done;
	c;;

(*compares two numbers*)
let f a b =
	if  a > b then 1 else -1;;

 (*move snake*)
 let move_snake snake =
  inc_snake snake 1;
  snake.len <- snake.len - 1;
  let l = snake.cur in
  let n = Array.length l - 1 in
  for q = 0 to 0 do
  if dis2 l n = 1 then begin snake.cur <- rem l; snake.tx <- ptx l.(n-1); snake.ty <- pty l.(n-1);end
else begin if ptx l.(n) = ptx l.(n-1) then begin snake.ty <- snake.ty + (f (pty(l.(n-1))) (pty(l.(n))) ); end
else begin snake.tx <- snake.tx + (f (ptx(l.(n-1))) (ptx(l.(n))) ); end end
done;
l.(n) <- snake.tx , snake.ty;;

(*checks whether snake is alive or not*)
let snake_alive snake = 
	let a = snake.hx  in
	let b = snake.hy  in
	if a < 8 || a > size_x() - 4 || b < 8 || b > size_y() -8 then false
else if check_point snake a b 2 then true
else false;;

(*game over function*)
let game_over snake =
	clear_graph();
	set_color blue;
	moveto 480 240;
	set_font "-misc-dejavu sans mono-bold-r-normal--256-0-0-0-m-0-iso8859-1";
	draw_string ("GAME OVER"^" AND YOUR SCORE WAS '"^(string_of_int (snake.len - 5))^"'");
	sound 1000 2;;

(*remove snake*)
let rems snake = 
set_color white;
set_line_width 5;
 draw_poly_line (snake.cur);
 set_line_width 1;;

(*move snake with keyboard interaction*)
let move_vel snake food q=
open_graph "";
	set_color blue;
	moveto 480 240;
	set_font "-misc-dejavu sans mono-bold-r-normal--256-0-0-0-m-0-iso8859-1";
	draw_string "PRESS 2 or 4 or 8 or 6 TO START";
	snake.dir <- read_key();
	clear_graph ();
	let temp1 = ref '0' in
	let s = ref '0' in
	let temp = ref true in
              while snake.alive do
              while key_pressed()= false && !temp do
                let t=Sys.time() in
                for g = 0 to 4 do
                while Sys.time () -.t <= !q do
                () done;
              move_snake snake ;
              auto_synchronize true;
              show_snake snake;
              show_food food;
              auto_synchronize false;
              rems snake; done;
              eat_food snake food !s q;
              temp := snake_alive snake; done;
              temp1 := read_key();
              for j= 0 to 0 do
              let s = ref (snake.dir) in
              match !temp1 with
              | x -> if ((x = '2'&& !s != '8') || ( x = '4'&& !s != '6') || (x = '6'&& !s != '4') || (x = '8'&& !s != '2')) then begin snake.dir <- !temp1; end else begin (); end done; done;
      game_over snake;;

let start() = 
open_graph "";
let snake = {len = 5; hx = 20; hy=10; ty=15;tx=10; cur=[|(20,10);(15,10)|]; col = blue; alive = true; dir = '6'} in
let food = {posx = (((Random.int (size_x()-5))/5)*5 + 5); posy = (((Random.int (size_y()-5))/5)*5 + 5) } in
move_vel snake food (ref 0.15);;

let snake = {len = 5; hx = 20; hy=10; ty=10;tx=15; cur=[|(20,10);(15,10)|]; col = blue; alive = true; dir = '6'};;
let test snake = 
	open_graph "";
	let food = {posx = (((Random.int (size_x()-5))/5)*5 + 5); posy = (((Random.int (size_y()-5))/5)*5 + 5) } in
move_vel snake food (ref 0.15);;