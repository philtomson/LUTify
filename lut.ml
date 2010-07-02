let rnd num = 
  let num_int = int_of_float num in
  if (num -. ( float_of_int  num_int  )) >= 0.5 then 
     num_int  + 1 
  else
     num_int ;;

let float_part n = n -. ( float_of_int ( int_of_float( n))) ;;

let idx_range n = int_of_float(n) , ceil n ;;


let make_lut f x0 x1 step = 
  let range = x1 -. x0 in
  let range_per_step = range /. step in
  let _ = Printf.printf "range_per_step is: %f\n" range_per_step in
  let cslots = int_of_float( ceil (range /. step )) in
  let _ = Printf.printf "cslots is: %d\n" cslots in
  let sslots = (rnd (range /. step)) in 
  let _ = Printf.printf "sslots is: %d\n" sslots in

  let slots = if (cslots =  sslots) then (cslots + 1) else cslots in (*TODO*)
  let _ = Printf.printf "slots is: %d\n" slots in
  let lut   = Array.make (slots )  (f x0) in
  let rec loop xn xc = match xn with
     n  when (n >= slots) -> ()
   | _ -> Printf.printf "xn is: %d => %f\n " xn (f xc); lut.(xn) <- (f xc ); loop (xn + 1) (xc +. step) in
  loop 0 x0 ;  (lut , (fun x -> (Printf.printf "idx is: %f\n" ((x -. x0)/. step)) ;   
                                lut.(rnd((x -. x0)/. step ))));;

(* probably best not to have linear interpolation for now :
let make_lut' f x0 x1 step = 
  let range = x1 -. x0 in
  let cslots = int_of_float( ceil (range /. step )) in
  let sslots = int_of_float(range /. step) in
  let slots = if cslots =  sslots then (cslots + 1) else cslots in
  let _ = Printf.printf "slots is: %d\n" slots in
  let lut   = Array.make slots ( f x0) in
  let rec loop idx xc = match idx with
     n  when (n >= slots) -> ()
   | _ -> Printf.printf "idx is: %d\n " idx; lut.(idx) <- (f xc ); loop (idx + 1) (xc +. step) in
  loop 0 x0 ;  
  (lut , (fun x  -> let indexf = ((x -. x0)/. step) in 
                    let _      = (Printf.printf "idx is: %f\n" indexf) in
                    if float_part indexf > 0.0 then
                     (*linear interpolation*)
                     (* this is broke - we're not getting the correct xa and xb
                        need a way to determine what they should be.  If x is not
                        directly mapped in the LUT then we want to choose a higher and
                        lower x that are and then interpolate between them. *)
                     let xa = float_of_int (int_of_float indexf) in
                     let xb = ceil indexf in
                     let ya = lut.(int_of_float xa) in
                     let yb = lut.(int_of_float xb) in
                     let _  = (Printf.printf "%f + (%f - %f)*((%f-%f)/(%f-%f))" ya x xa yb ya xb xa ) in
                     ya +. (x -. xa)*.((yb -. ya)/.(xb -. xa))

                    else lut.(rnd indexf) 
           ) 
  )         ;; 
  *)

let pi = 3.1415927 in
let sinlut, sinlutfn = make_lut (sin) 0.0 (pi/. 2.0) 0.01 in
let evenlut, evenlutfn = make_lut (fun x->x) 0.0 10.0 2.0 in
let evenlut', evenlutfn' = make_lut (fun x->x) 0.0 11.0 2.0 in
let sinlut', sinlutfn' = make_lut (sin) 0.0 (pi/. 2.0) 0.1 in

Printf.printf "sin pi/2 is: (from fn) %f (from lut): %f\n" (sinlutfn' (pi/. 2.0)) sinlut'.(7);
;;


