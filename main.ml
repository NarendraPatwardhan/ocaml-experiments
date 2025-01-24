(* Define the instruction set *)
type instruction =
  | Push of int        (* Push a value onto the stack *)
  | Pop                (* Pop the top value from the stack *)
  | Inspect            (* Inspect the top value on the stack *)
  | Add                (* Add the top two values on the stack *)
  | Sub                (* Subtract the top two values on the stack *)
  | Mul                (* Multiply the top two values on the stack *)
  | Div                (* Divide the top two values on the stack *)
  | Mod                (* Modulo the top two values on the stack *)
  | And                (* Bitwise AND the top two values on the stack *)
  | Or                 (* Bitwise OR the top two values on the stack *)
  | Xor                (* Bitwise XOR the top two values on the stack *)
  | Not                (* Bitwise NOT the top value on the stack *)
  | Load of int        (* Load a value from memory onto the stack *)
  | Store of int       (* Store the top value from the stack into memory *)

(* Define the VM state *)
type vm = {
  stack : int list;          (* The stack of integers *)
  program : instruction list; (* The program to execute *)
  memory : int array;        (* Linear memory model *)
  call_stack : int list;     (* Call stack for function calls *)
  pc : int;                  (* Program counter *)
}

(* Initialize the VM *)
let init_vm program memory_size = {
  stack = [];
  program = program;
  memory = Array.make memory_size 0;
  call_stack = [];
  pc = 0;
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
  | Pop ->
      let (_, stack') = pop vm.stack in
      { vm with stack = stack' }
  | Inspect ->
      let (value, _) = pop vm.stack in
      Printf.printf "Top of stack: %d\n" value;
      vm
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
  | Mod ->
      let (a, stack') = pop vm.stack in
      let (b, stack'') = pop stack' in
      { vm with stack = push stack'' (b mod a) }
  | And ->
      let (a, stack') = pop vm.stack in
      let (b, stack'') = pop stack' in
      { vm with stack = push stack'' (a land b) }
  | Or ->
      let (a, stack') = pop vm.stack in
      let (b, stack'') = pop stack' in
      { vm with stack = push stack'' (a lor b) }
  | Xor ->
      let (a, stack') = pop vm.stack in
      let (b, stack'') = pop stack' in
      { vm with stack = push stack'' (a lxor b) }
  | Not ->
      let (a, stack') = pop vm.stack in
      { vm with stack = push stack' (lnot a) }
  | Load addr ->
      let value = vm.memory.(addr) in
      { vm with stack = push vm.stack value }
  | Store addr ->
      let (value, stack') = pop vm.stack in
      vm.memory.(addr) <- value;
      { vm with stack = stack' }

(* Run the VM *)
let rec run_vm vm =
  if vm.pc >= List.length vm.program then
    vm (* No more instructions to execute *)
  else
    let instr = List.nth vm.program vm.pc in
    let vm' = execute_instruction vm instr in
    run_vm { vm' with pc = vm'.pc + 1 }

(* Helper function to print the stack *)
let print_stack vm =
  List.iter (Printf.printf "%d ") vm.stack;
  print_newline ()

(* Helper function to print memory *)
let print_memory vm =
  Array.iteri (fun i value -> Printf.printf "Memory[%d] = %d\n" i value) vm.memory

(* Example usage *)
let () =
  (* Define a simple program: (2 + 3) * 4 *)
  let program = [
    Push 2;
    Push 3;
    Add;
    Push 4;
    Mul;
    Store 0; (* Store result in memory[0] *)
    Load 0;  (* Load result from memory[0] *)
  ] in

  (* Initialize and run the VM *)
  let vm = init_vm program 10 in
  let result_vm = run_vm vm in

  (* Print the final stack and memory *)
  print_stack result_vm;
  print_memory result_vm
