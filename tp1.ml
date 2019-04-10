type instruction =
        |Reset of int
        |Incr of int
        |Set of int*int
        |Jump of int*int*int

type programme = instruction array

let max_registers = 1024
let max_instructions = 1048576

exception Memory_exhausted
exception Segmentation_fault
exception Ressources_exhausted

let run_instruction inst i reg =
        match inst with
        |Reset(k) -> if k >= max_registers then
                raise Memory_exhausted
        else
                let _ = reg.(k) <- 0 in i + 1
        |Incr(k) ->  if k >= max_registers then
                raise Segmentation_fault
        else
                let _ = reg.(k) <- reg.(k) + 1 in i + 1
        |Set(k1, k2) -> if (k1 >= max_registers || k2 >= max_registers) then
                raise Memory_exhausted
        else
                let _ = reg.(k1) <- reg.(k2) in i + 1
        |Jump(r1, r2, i3) -> if (r1 >= max_registers|| r2 >= max_registers) then
                raise Segmentation_fault
        else
                if reg.(r1) = reg.(r2) then 
                        i3
                else
                        i+1

let rec step reg p mi nbi i =
        if nbi > mi then 
                raise Ressources_exhausted
        else if (i >= Array.length p) then 
                ()
        else 
                let j = run_instruction p.(i) i reg in
                let _ = step reg p mi (nbi+1) j in ()

let step_program reg p mi =
        let _ = step reg p mi 0 0 in
        reg

let run_function p x =
        let reg = Array.make max_registers 0 in
        let _ = reg.(0) <- x in
        let _ = step_program reg p max_instructions in
        reg.(0)

let normalize p =
        let ind_max = (Array.length p) - 1 in
        let p2 = (Array.map (fun inst -> match inst with 
        |Reset(k) -> Reset(k)
        |Incr(k) -> Incr(k)
        |Set(i, j) -> Set(i, j)
        |Jump(i, j, k) -> if k > ind_max then 
                Jump(i, j, ind_max+1)
        else Jump(i, j, k)) p) in
        p2

let rho p =
        Array.fold_left (fun m inst -> match inst with
        |Reset(k)
        |Incr(k)
        |Set(k, _) -> max k m
        |_ -> m) 0 p

let clean_program p = 
        let init = Array.make (rho p - 1) (Reset(1)) in
        let initv2 = Array.mapi (fun i inst -> Reset(i+1)) init in
        Array.append p initv2

let print_prog p =
        Array.iteri (fun i inst -> match inst with 
        |Reset(k) -> Printf.printf "%d. Reset(%d) \n" i k
        |Incr(k) -> Printf.printf "%d. Incr(%d) \n" i k
        |Set(j, k) -> Printf.printf "%d. Set(%d, %d) \n" i j k
        |Jump(j, k, l) -> Printf.printf "%d. Jump(%d, %d, %d) \n" i j k l) p

let string_of_prog p = 
        let rec aux i p =
                if i >= Array.length p then ""
                else 
                        let s = match p.(i) with 
                        |Reset(k) -> Printf.sprintf "%d. Reset(%d) \n" i k
                        |Incr(k) -> Printf.sprintf "%d. Incr(%d) \n" i k
                        |Set(j, k) -> Printf.sprintf "%d. Set(%d, %d) \n" i j k
                        |Jump(j, k, l) -> Printf.sprintf "%d. Jump(%d, %d, %d) \n" i j k l
                        in
                        Printf.sprintf "%s%s" s (aux (i+1) p)
        in
        aux 0 p

let compose p1 p2 =
        Array.append p1 (Array.map (fun inst -> match inst with
        |Jump(j, k, l) -> Jump(j, k, (Array.length p1) + l)
        |_ -> inst) p2)

let somme x y =
        let reg = Array.make max_registers 0 in
        let _ = reg.(0) <- x in
        let _ = reg.(1) <- y in
        let p = [| Set(2, 0); 
                    Jump(1, 3, 5); 
                    Incr(3); 
                    Incr(2); 
                    Jump(1, 1, 1); 
                    Set(0,2)|] in
        let _ = print_prog p in
        let _ = step_program reg p max_instructions in
        reg.(0)

let translate p indice_reg r =
        let rh = rho p in
        let n = Array.length indice_reg in
        if n > rh then failwith "translate : n > rho(p)\n" else
        let truc = Array.mapi (fun i ind -> Set(i, ind)) indice_reg in
        let p1 = Array.make (n - rh) (Reset(0)) in
        let p1 = Array.mapi (fun i inst -> Reset(n+i)) p1 in
        let truc = Array.append truc p1 in
        let p2 = compose truc p in
        let p3 = Array.append p2 [| Set(r, 0) |] in 
        p3

(*let compose2 f g *)

let debug_program reg p mi =
        let rh = rho p in
        let _ = Array.iteri (fun i r -> if i <= rh then Printf.printf "%d. %d \n" i r else ()) in
        let _ = step reg p mi 0 0 in
        let _ = Array.iteri (fun i r -> if i <= rh then Printf.printf "%d. %d \n" i r else ())in
        reg

let _ = Printf.printf "%d\n" (somme 85 2)


