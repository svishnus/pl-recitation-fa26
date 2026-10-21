let count = ref 0

let first () =
  count := !count + 1;
  Printf.printf "[first]  count = %d\n" !count;
  10

let second () =
  count := !count + 10;
  Printf.printf "[second] count = %d\n" !count;
  20

let print_sum a b =
  Printf.printf "Result of a + b: %d\n" (a + b)

let () =
  Printf.printf "=== Demonstration 1: Function Argument Evaluation Order ===\n";
  print_sum (first ()) (second ());

  Printf.printf "\n=== Demonstration 2: Expression Operand Evaluation Order ===\n";
  let total = first () + second () in
  Printf.printf "Total: %d\n" total
