(** The main paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES, and PROGRAM STATE   *)
(******************************************)

(** A location in the paint_canvas widget *)
type point = position (* from Gctx *)

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
(* TODO: You will modify this definition in Tasks 3, 4, 5 and maybe 6. *)
type shape = 
  | Line of {color: color; p1: point; p2: point; thickness: thickness}
  | Points of {color: Gctx.color; points: point list; thickness: thickness}
  | Ellipse of {color: Gctx.color; c: point; rx: int; ry: int; 
                thickness: thickness}

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

      - LineStartMode means the paint program is waiting for the user to make
        the first click to start a line.

      - LineEndMode means that the paint program is waiting for the user's
        second click. The point associated with this mode stores the location of
        the user's first mouse click.  *)
(* TODO: You will need to modify this type in Tasks 2, 4, and maybe 6. *)
type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseStartMode
  | EllipseEndMode of point


(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;

  (* TODO: You will need to add new state for Tasks 2, 3, 5, and *)
  (* possibly 6 *) 
  mutable preview : shape option;
  
  mutable thickness : thickness;
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  (* TODO: You will need to add new state for Tasks 2, 3, 5, and maybe 6 *)
  preview = None;
  thickness = thin
}



(** This function creates a graphics context with the appropriate
    pen color.
*)
(* TODO: Your will need to modify this function in Task 5 *)
let with_params (g: gctx) (c: color) (t: thickness): gctx =
  let g = with_color g c in
  let g = with_thickness g t in
  g


(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)

(* TODO: You will need to modify this repaint function in Tasks 2, 3,
   4, and possibly 5 or 6. For example, if the user is performing some
   operation that provides "preview" (see Task 2) the repaint function
   must also show the preview.  *)
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line l -> draw_line (with_params g l.color l.thickness) l.p1 l.p2
      | Points p -> draw_points (with_params g p.color p.thickness) p.points
      | Ellipse e -> draw_ellipse (with_params g e.color e.thickness) 
                                  e.c e.rx e.ry
    end in
  let preview_shape (x: shape option) : unit =
     begin match x with
     | None -> ()
     | Some (Line l) -> draw_shape (Line l)
     | Some (Points p) -> draw_shape (Points p)
     | Some (Ellipse e) -> draw_shape (Ellipse e)
     end in
  Deque.iterate draw_shape paint.shapes;
  preview_shape paint.preview
  
  
(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint


(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)


(* Returns midpoint given two positions *)
let get_center (r1: position) (r2: position): point =
    let (x1, y1) = r1 in
    let (x2, y2) = r2 in
    let x = (x1 + x2) / 2 in
    let y = (y1 + y2) / 2 in
    (x, y)  

(* Returns x and y radii of ellipse given two positions *)
let get_rs (r1: position) (r2: position) : (int*int) =
    let (x1, y1) = r1 in
    let (x2, y2) = r2 in
    (abs (x2 - x1), abs (y2 - y1))

(** The paint_action function processes all events that occur 
    in the canvas region. *)
(* TODO: Tasks 2, 3, 4, 5, and 6 involve changes to paint_action. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
       (* This case occurs when the mouse has been clicked in the
          canvas, but before the button has been released. How we
          process the event depends on the current mode of the paint
          canvas.  *)
      (begin match paint.mode with 
         | LineStartMode ->
           (* The paint_canvas was waiting for the first click of a line,   
              so change it to LineEndMode, recording the starting point of  
              the line. *)
           paint.mode <- LineEndMode p
         | LineEndMode _ -> ()
         | PointMode -> let p1 = Points {color = paint.color; points = [p];
                                         thickness = paint.thickness} in
                         paint.preview <- Some p1
         | EllipseStartMode -> paint.mode <- EllipseEndMode p
         | EllipseEndMode _ -> ()
         (* The paint_canvas was waiting for the second click of a line,  
            so create the line and add it to the deque of shapes. Go back 
            to waiting for the first click. *)
       end)
    | MouseDrag ->
      (* In this case, the mouse has been clicked, and it's being dragged    
         with the button down. Initially there is nothing to do, but you'll  
         need to update this part for Task 2, 3, 4 and maybe 6. *)
         (begin match paint.mode with
           | LineStartMode -> ()
           | LineEndMode p1 -> paint.preview <- Some (Line {color = paint.color;
                               p1 = p1; p2 = p; thickness = paint.thickness})
           | PointMode -> let points =
                            begin match paint.preview with
                            | Some (Points ps) -> ps.points
                            | _ -> []
                            end in
                          let p2 = p::points in
                          let p3 = Points {color = paint.color;points = p2;
                                           thickness = paint.thickness} in
                          paint.preview <- Some p3
           | EllipseStartMode -> ()
           | EllipseEndMode p1 -> let e1 = Ellipse {color = paint.color;
                                                 c = get_center p p1; 
                                                 rx = (fst (get_rs p p1)) / 2; 
                                                 ry = (snd (get_rs p p1)) / 2;
                                                 thickness = paint.thickness} 
                                   in paint.preview <- Some e1
                                  
           end)
                        
    | MouseUp ->
      (* In this case there was a mouse button release event. TODO: Tasks 2, *)
      (* 3, 4, and possibly 6 need to do something different here.           *)
     (begin match paint.mode with
       | LineStartMode -> ()
       | LineEndMode p1 ->
         Deque.insert_tail (Line {color = paint.color; p1 = p1; p2 = p;
                                  thickness = paint.thickness}) 
                          paint.shapes;
         paint.preview <- None;
         paint.mode <- LineStartMode
       | PointMode -> let points =
                       begin match paint.preview with
                            | Some (Points ps) -> ps.points
                            | _ -> []
                            end in
                       Deque.insert_tail (Points {color = paint.color;
                                          points = points; 
                                          thickness = paint.thickness}) 
                                          paint.shapes;
                       paint.preview <- None
        | EllipseStartMode -> ()
        | EllipseEndMode p1 -> Deque.insert_tail 
                                  (Ellipse {color = paint.color;
                                            c = get_center p p1; 
                                            rx = (fst (get_rs p p1)) / 2; 
                                            ry = (snd (get_rs p p1)) / 2;
                                            thickness = paint.thickness}) 
                                   paint.shapes;
                              paint.preview <- None;
                              paint.mode <- EllipseStartMode
      end)
    
    | _ -> ()
    (* This catches the MouseMove event (where the user moved the mouse over  
       the canvas without pushing any buttons) and the KeyPress event (where 
       the user typed a key when the mouse was over the canvas). *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action


(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** This part of the program creates the other widgets for the
   paint program -- the buttons, color selectors, etc., and
   lays them out in the top - level window. *)
(* TODO: Tasks 1, 2, 4, 5, and 6 involving adding new buttons or
   changing the layout of the Paint GUI. Initially the layout is very
   ugly because we use only the hpair widget demonstrated in
   Lecture. Task 1 is to make improvements to make the layout more
   appealing. You may choose to arrange the buttons and other GUI
   elements of the paint program however you like (so long as it is
   easily apparent how to use the interface ).  The sample screen shot
   of our solution provides one possible design.  Also, feel free to
   improve the visual components of the GUI, for example, our solution
   puts borders around the buttons and uses a custom "color button"
   that changes its appearance based on whether or not the color is
   currently selected.  *)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"

(** This function runs when the Undo button is clicked.
   It simply removes the last shape from the shapes deque. *)
(* TODO: You need to modify this in Task 2, 3 and 4. *)
let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes)

;; nc_undo.add_event_listener (mouseclick_listener undo)

let (w_lines, lc_lines, nc_lines) = button "Lines"

let lines () : unit =
  paint.mode <- LineStartMode
  
;; nc_lines.add_event_listener (mouseclick_listener lines)

let (w_points, lc_points, nc_points) = button "Points"

let points () : unit =
  paint.mode <- PointMode
  
;; nc_points.add_event_listener (mouseclick_listener points)

let (w_ellipses, lc_ellipses, nc_ellipses) = button "Ellipses"

let ellipses () : unit =
   paint.mode <- EllipseStartMode
   
;; nc_ellipses.add_event_listener (mouseclick_listener ellipses)

(* Creates a checkbox widget *)
let (w_thick, vc_thick) = checkbox false "Thick Lines"

(* Adds a change listener to change the thickness of the paint if the 
   box is checked *)
;; vc_thick.add_change_listener (fun t -> if t then 
                                 paint.thickness <- thick
                                 else paint.thickness <- thin)

(* Creates a slider widget *)
let (w_ths, vc_ths) = slider 1

(* Adds a change listener to change the thickness accordingly if the 
   thickness checkbox is checked *)
;; vc_ths.add_change_listener (fun t -> if vc_thick.get_value()
                               then paint.thickness <- {t})


(** A spacer widget *)
let spacer : widget = space (10,10)


(** The mode toolbar, initially containing just the Undo button. *)
(*  TODO: you will need to add more buttons to the toolbar in 
    Tasks 3, 5, and possibly 6. *)
let mode_toolbar : widget =
  let ws = [spacer; hpair (border w_lines) spacer; 
            hpair (border w_points) spacer;
            hpair (border w_ellipses) spacer;
            border (hpair w_thick w_ths);
            spacer;
            border w_undo] in
  hlist ws
  
(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color 
   and some buttons for changing it. Both the indicator and the buttons 
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given 
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, 0) (width-1, width-1) in   
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected 
    color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created 
    with. They are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)  
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c ));
  w


(** The color selection toolbar. Contains the color indicator and 
    buttons for several different colors. *)
   let color_toolbar : widget =
   let ws = [spacer; hpair (color_button black) spacer; 
             hpair (color_button white) spacer; 
             hpair (color_button red) spacer; 
             hpair (color_button green) spacer; 
             hpair (color_button blue) spacer; 
             hpair (color_button yellow) spacer; 
             hpair (color_button cyan) spacer; 
             hpair (color_button magenta) spacer;
             color_indicator] in
   hlist ws
   


(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets.
*)
(* TODO: Task 1 (and others) involve modifing this layout to add new
   buttons and make the layout more aesthetically appealing. *)
let paint_widget =
  let ws = [paint_canvas; spacer; mode_toolbar; spacer; color_toolbar] in
  vlist ws
  


(**************************************)
(**      Start the application        *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
