open Core



let prefix_sum (lst, m) i =
  let current_max = Int.max m (int_of_char i - int_of_char '0') in
  (* Printf.printf "Current Max: %d\n" current_max; *)
  (current_max :: lst, current_max)

let part_one filename =
  try
  let sum = ref 0 in
    In_channel.with_file filename ~f:(fun channel ->
     In_channel.iter_lines channel ~f:(fun line ->
       let line_split = String.to_list line in
       let max_left, _ = List.fold_left ~init:([], 0) ~f:prefix_sum line_split in

       let max_right, _  = List.fold_right ~init:([], 0) ~f:(fun i (lst, m) -> prefix_sum (lst, m) i) line_split in

       let last_max_right = List.hd_exn max_right in

       let max_left = List.rev max_left in

       let max_right = last_max_right :: max_right in

       List.iter ~f:(fun x -> Printf.printf "%d," x) max_left;
       Printf.printf "\n";
       List.iter ~f:(fun x -> Printf.printf "%d," x) max_right;

       let cell_max = List.fold2_exn ~init:0 ~f:(fun i left right ->
         (* Printf.printf "Left %d\n" left;
         Printf.printf "Right %d\n" right; *)
         let n = (left * 10) + right in
         Int.max n i
       ) max_left max_right in
       Printf.printf "Max: %d\n" cell_max;
       sum := !sum + cell_max;
     )
    );
    print_endline (string_of_int !sum);
  with
  | Sys_error msg ->
    eprintf "Error: %s\n%!" msg