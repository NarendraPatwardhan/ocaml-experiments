(* Exceptions for error conditions and control‐flow signals *)
exception VM_Error of string
exception Break

(* The instruction set *)
type instr =
  (* Stack manipulation *)
  | Push of int
  | Drop
  | Dup
  | Swap
  (* Arithmetic *)
  | Plus | Minus | Mul | Div | Mod
  (* Logic *)
  | Eq | Lt | Gt
  (* Boolean *)
  | And | Or | Not
  (* Debug *)
  | Inspect
  (* Structured control flow. In our “structured” VM the compound instructions
     contain lists of instructions. End is implicit (the end of the list). *)
  | Do of instr list     (* executed once *)
  | Loop of instr list      (* repeatedly executed until a Break *)
  | If of instr list * instr list option  (* optional else branch *)
  | Break                   (* break out one level *)
  (* Memory (we assume a linear memory array) *)
  | Load of int             (* load from memory address *)
  | Store of int            (* store into memory address *)

(* The machine state: a stack (of ints) and a fixed-size memory *)
type state = {
  stack : int list;
  memory : int array;
}

(* Initial state: empty stack and memory of 1024 cells initialized to 0 *)
let init_state () = { stack = []; memory = Array.make 1024 0 }

(* Basic stack helpers *)
let pop st =
  match st.stack with
  | [] -> raise (VM_Error "pop on empty stack")
  | x :: xs -> (x, { st with stack = xs })

let push x st = { st with stack = x :: st.stack }

(* We use a helper to drop n elements from a list. *)
module ListExt = struct
  let rec drop n l =
    if n <= 0 then l else
      match l with
      | [] -> []
      | _::xs -> drop (n-1) xs
end

(* The interpreter: it executes a list of instructions sequentially.
   Note: in a real VM one might compile to an array with explicit program counter,
   but here we use a recursive function over the list. *)
let rec exec (prog : instr list) (st : state) : state =
  let rec run prog st =
    match prog with
    | [] -> st
    | i :: rest ->
      let st' =
        match i with
        (* --- Stack manipulation --- *)
        | Push n -> push n st
        | Drop ->
            let (_, st') = pop st in st'
        | Dup ->
            let (x, _) = pop st in push x st
        | Swap ->
            let (x, st1) = pop st in
            let (y, st2) = pop st1 in
            { st2 with stack = x :: y :: st2.stack }
        (* --- Arithmetic --- *)
        | Plus ->
            let (x, st1) = pop st in
            let (y, st2) = pop st1 in
            push (y + x) st2
        | Minus ->
            let (x, st1) = pop st in
            let (y, st2) = pop st1 in
            push (y - x) st2
        | Mul ->
            let (x, st1) = pop st in
            let (y, st2) = pop st1 in
            push (y * x) st2
        | Div ->
            let (x, st1) = pop st in
            if x = 0 then raise (VM_Error "division by zero")
            else
              let (y, st2) = pop st1 in
              push (y / x) st2
        | Mod ->
            let (x, st1) = pop st in
            if x = 0 then raise (VM_Error "modulo by zero")
            else
              let (y, st2) = pop st1 in
              push (y mod x) st2
        (* --- Logic --- *)
        | Eq ->
            let (x, st1) = pop st in
            let (y, st2) = pop st1 in
            push (if x = y then 1 else 0) st2
        | Lt ->
            let (x, st1) = pop st in
            let (y, st2) = pop st1 in
            push (if y < x then 1 else 0) st2
        | Gt ->
            let (x, st1) = pop st in
            let (y, st2) = pop st1 in
            push (if y > x then 1 else 0) st2
        (* --- Boolean --- *)
        | And ->
            let (x, st1) = pop st in
            let (y, st2) = pop st1 in
            push (if x <> 0 && y <> 0 then 1 else 0) st2
        | Or ->
            let (x, st1) = pop st in
            let (y, st2) = pop st1 in
            push (if x <> 0 || y <> 0 then 1 else 0) st2
        | Not ->
            let (x, st1) = pop st in
            push (if x = 0 then 1 else 0) st1
        (* --- Debug --- *)
        | Inspect ->
            List.iter (fun n -> Printf.printf "%d " n) st.stack;
            Printf.printf "\n";
            st
        (* --- Control Flow --- *)
        | Do body ->
            let st' = run body st in
            st'
        | Loop body ->
            let rec loop st =
              try
                let st' = run body st in
                loop st'
              with Break -> st
            in
            loop st
        | If (tbranch, fbranch) ->
            let (cond, st1) = pop st in
            if cond <> 0 then run tbranch st1
            else
              (match fbranch with
               | None -> st1
               | Some fb -> run fb st1)
        | Break ->
            (* Break is meant to abort the innermost loop/do *)
            raise Break
        (* --- Memory --- *)
        | Load addr ->
            if addr < 0 || addr >= Array.length st.memory then
              raise (VM_Error "invalid memory load")
            else
              push st.memory.(addr) st
        | Store addr ->
            let (x, st1) = pop st in
            if addr < 0 || addr >= Array.length st.memory then
              raise (VM_Error "invalid memory store")
            else
              (st.memory.(addr) <- x; st1)
      in
      run rest st'
  in
  run prog st

(* An example program to test the VM *)
let prog = [
  (* Push two numbers and add them *)
  Push 10; Push 20; Plus; Inspect;
  (* Execute a block once *)
  Do [Push 1; Inspect];
  (* Execute a loop that breaks immediately *)
  Loop [
    Push 0; Inspect;
    Break;  (* This break will exit the loop *)
    Push 999; Inspect  (* This instruction is never reached *)
  ];
  (* An if with an else branch *)
  Push 0;
  If ([Push 100; Inspect],
      Some [Push 200; Inspect]);
  Push 42; Inspect;
]

let () =
  let _ = exec prog (init_state ()) in
  ()

