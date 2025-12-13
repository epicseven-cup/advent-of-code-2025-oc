open Core

let compare_diff num base =
  let left = ref num in
  let right =  ref 0 in

  for _ = 1 to base / 2 do
    let m = !left mod 10 in
    left := !left - m;
    right := (!right * 10) + m
  done;
  !left = !right

let compute_range start ending =
  let range_length = ending - start in
  let full_range = List.init range_length ~f: (fun i -> start + i) in
  List.filter ~f: (fun current ->
    let s = Float.to_int (Float.log10 (Int.to_float current))in
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

  let start = safetly_parse_list_string start_str in
  let ending = safetly_parse_list_string ending_str in
 (start, ending)


let part_one filename =
  try
    let ranges = String.split ~on:',' (In_channel.read_all filename) in
    let current = ref 0 in
    List.iter ranges ~f:(fun inp ->
        let (start, ending) = compute_range_entry inp in
        let current_range = compute_range start ending in
        let range_sum = List.fold_left ~f:(+) ~init: 0 current_range in
        current := !current + range_sum;
    );
    print_endline ("ans:" ^ string_of_int (!current));
  with
  | Sys_error msg -> eprintf "Error: %s\n%!" msg;
