open Core

let compare_diff num base =
  let left = ref num in
  let right =  ref 0 in

  let midpoint = base / 2 in
  let division = Float.to_int(10. ** (Float.of_int midpoint )) in
  right := !left mod (division);

  left := (!left - !right)/ (division);
(*
  Printf.printf "Left: [%d]\n" !left;
  Printf.printf "Right: [%d]\n" !right; *)

  !left = !right


let compare_diff_all_pattern num base =
  let comp = ref false in
  let base_end = if base = 2 then 1 else base - 2 in
  let total_num = (Float.to_int (Float.log10 (Int.to_float num))) + 1 in
  for i = 1 to base_end do
    let division = Float.(/) (Int.to_float total_num) (Int.to_float i) in
    (* Printf.printf "total_num: %f\n" (division); *)
    if  Float.is_integer division then
    (* Printf.printf "division: %d\n" division; *)
      let current_pattern = ref Int.Set.empty in
      let n = Float.to_int(10. ** (Float.of_int i)) in
      let copy_num = ref num in
      (* Printf.printf "Copy num: %d\n" !copy_num; *)
      (* Printf.printf "N: %n\n" n; *)
      while !copy_num > 0 do
        (* Printf.printf "Current Pattern: [%d]\n" !copy_num; *)
        (* Set.iter ~f: (fun i -> Printf.printf "Set: [%d]\n" i) !current_pattern; *)
        let m = !copy_num mod n in
        current_pattern := Set.add !current_pattern m;
        copy_num := (!copy_num - m) / n;
      done;
      (* Set.iter ~f: (fun i -> Printf.printf "Final Set: [%d]\n" i) !current_pattern; *)
      if Set.length !current_pattern = 1 then comp := true;
  done;
  !comp


let compute_range_two start ending =
  let range_length = (ending - start) + 1 in
  let full_range = List.init range_length ~f: (fun i -> start + i) in
  List.filter ~f: (fun current ->
    let base = (Float.to_int (Float.log10 (Int.to_float current))) + 1 in
    (* Printf.printf "Base: [%d]\n" base; *)
    compare_diff_all_pattern current base
  ) full_range


let compute_range start ending =
  let range_length = (ending - start) + 1 in
  let full_range = List.init range_length ~f: (fun i -> start + i) in
  List.filter ~f: (fun current ->
    let s = (Float.to_int (Float.log10 (Int.to_float current))) + 1 in
    s mod 2 = 0 && compare_diff current s
  ) full_range



let safetly_parse_list_string num_str =
  match num_str with
  | Some s ->
    int_of_string (String.strip s)
  | None -> failwith "num_str is None"

let compute_range_entry range_line =
  let range = String.split ~on:'-' range_line in
  let start_str = List.nth range 0 in
  let ending_str = List.nth range 1 in

  Printf.printf "start_str: [%s]\n%!" (Option.value_exn start_str);
  Printf.printf "ending_str: [%s]\n%!" (Option.value_exn ending_str);

  let start = safetly_parse_list_string start_str in
  let ending = safetly_parse_list_string ending_str in
 (start, ending)


let part_one filename = (
  try
    let ranges = String.split ~on:',' (In_channel.read_all filename) in
    let current = ref 0 in
    List.iter ranges ~f:(fun inp ->
        let (start, ending) = compute_range_entry inp in
        let current_range = compute_range start ending in
        let range_sum = List.fold_left ~f:(+) ~init: 0 current_range in
        List.iter ~f: (fun i -> Printf.printf "Range: [%d]\n" i) current_range;
        current := !current + range_sum;
    );
    print_endline ("ans:" ^ string_of_int (!current));
  with
  | Sys_error msg -> eprintf "Error: %s\n%!" msg;
)


let part_two filename =
  try
    let ranges = String.split ~on:',' (In_channel.read_all filename) in
    let current = ref 0 in
    List.iter ranges ~f:(fun inp ->
        let (start, ending) = compute_range_entry inp in
        let current_range = compute_range_two start ending in
        let range_sum = List.fold_left ~f:(+) ~init: 0 current_range in
        List.iter ~f: (fun i -> Printf.printf "Range: [%d]\n" i) current_range;
        current := !current + range_sum;
    );
    print_endline ("ans:" ^ string_of_int (!current));
  with
  | Sys_error msg -> eprintf "Error: %s\n%!" msg;