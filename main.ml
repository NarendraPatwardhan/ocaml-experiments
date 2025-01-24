(* Define the instruction set *)
type instruction =
  | Push of int        (* Push a value onto the stack *)
  | Add                (* Add the top two values on the stack *)
  | Sub                (* Subtract the top two values on the stack *)
  | Mul                (* Multiply the top two values on the stack *)
  | Div                (* Divide the top two values on the stack *)
  | Pop                (* Pop the top value from the stack *)

(* Define the VM state *)
type vm = {
  stack : int list;    (* The stack of integers *)
  program : instruction list; (* The program to execute *)
}

(* Initialize the VM *)
let init_vm program = {
  stack = [];
  program = program;
}

(* Push a value onto the stack *)
let push stack value = value :: stack

(* Pop a value from the stack *)
let pop stack =
  match stack with
  | [] -> failwith "Stack underflow"
  | x :: xs -> (x, xs)

(* Execute a single instruction *)
let execute_instruction vm instr =
  match instr with
  | Push value -> { vm with stack = push vm.stack value }
  | Add ->
      let (a, stack') = pop vm.stack in
      let (b, stack'') = pop stack' in
      { vm with stack = push stack'' (a + b) }
  | Sub ->
      let (a, stack') = pop vm.stack in
      let (b, stack'') = pop stack' in
      { vm with stack = push stack'' (b - a) }
  | Mul ->
      let (a, stack') = pop vm.stack in
      let (b, stack'') = pop stack' in
      { vm with stack = push stack'' (a * b) }
  | Div ->
      let (a, stack') = pop vm.stack in
      let (b, stack'') = pop stack' in
      { vm with stack = push stack'' (b / a) }
  | Pop ->
      let (_, stack') = pop vm.stack in
      { vm with stack = stack' }

(* Run the VM *)
let rec run_vm vm =
  match vm.program with
  | [] -> vm (* No more instructions to execute *)
  | instr :: rest ->
      let vm' = execute_instruction vm instr in
      run_vm { vm' with program = rest }

(* Helper function to print the stack *)
let print_stack vm =
  List.iter (Printf.printf "%d ") vm.stack;
  print_newline ()

(* Example usage *)
let () =
  (* Define a simple program: (2 + 3) * 4 *)
  let program = [
    Push 2;
    Push 3;
    Add;
    Push 4;
    Mul;
  ] in

  (* Initialize and run the VM *)
  let vm = init_vm program in
  let result_vm = run_vm vm in

  (* Print the final stack *)
  print_stack result_vm
