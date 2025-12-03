open Core


let convert_line_int line =  (int_of_string (String.sub line ~pos:1 ~len:(String.length line - 1) ))

(* let day_one_part_one filename =
  try
    let pass_through_zero = ref 0 in
    let location = ref 50 in
    In_channel.with_file filename ~f:(fun channel ->
      In_channel.iter_lines channel ~f:(fun line -> 

        (* let old_location = !location in  *)
        let move = convert_line_int line in
        if Char.equal line.[0] 'L' then 
          location := (!location - move)
        else location := !location + move;

        (* pass_through_zero := !pass_through_zero + (move / 100); *)
        location := (!location % 100);
        print_endline ( string_of_int !location);
        if !location = 0 then pass_through_zero := !pass_through_zero + 1

        (* if !location < old_location && !location > 0 then
          pass_through_zero := !pass_through_zero + 1 *)
      )
    );
    print_endline( string_of_int !pass_through_zero);
  with 
  | Sys_error msg ->
    eprintf "Error: %s\n%!" msg *)



let day_one_part_two filename =

 try
    let pass_through_zero = ref 0 in
    let location = ref 50 in
    In_channel.with_file filename ~f:(fun channel ->
      In_channel.iter_lines channel ~f:(fun line -> 

        let old_location = !location in 
        let move = ref (convert_line_int line) in
        print_endline( "move:" ^ string_of_int !move);
        

        if !move > 100 then (
          pass_through_zero := !pass_through_zero + (!move / 100);
          move := !move - ((!move / 100) * 100);
          print_endline( "after update move:" ^ string_of_int !move)
        )else();

        if Char.equal line.[0] 'L' then( 
          location := (!location - !move);
          if old_location < !move && old_location <> 0 then
            pass_through_zero := !pass_through_zero + 1)
        else ( 
          location := !location + !move;
          if 100 -old_location < !move && old_location <> 0 then
            pass_through_zero := !pass_through_zero + 1;
        );
        location := (!location % 100);

        if !location = 0 then pass_through_zero := !pass_through_zero + 1;

        print_endline( "pass:" ^ string_of_int !pass_through_zero);
        
      )
    );
    print_endline( string_of_int !pass_through_zero);
  with 
  | Sys_error msg ->
    eprintf "Error: %s\n%!" msg

let () = day_one_part_two "input.txt"