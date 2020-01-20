(* 
 * Change these three bindings to the directories of the input files (tests), output files 
 * (where your results will be written to), and expected output files (your results should
 * match these files) 
 *)
let test_cases_directory = "./cases" in
let expected_output_directory = "./expected" in
let your_output_directory = "./your_output" in

(*--------------------------------------------------------------------------------------------------*)

let run_diff = true in
(*
 * Writes all tokens from the next_tok stream to the out_stream
 *)
let write_tokens (next_tok : unit -> token) (out_stream : out_channel) : unit =
  let print_out = Printf.fprintf out_stream in  (* Curry Printf.fprintf to write to our file *)
  let rec write_token (tok : token) : unit =
    match tok with
    | CloseParenToken -> 
        print_out "CloseParenToken\n";
        write_token (next_tok ())
    | EndToken ->   (* Basis *)
        print_out "EndToken\n";
    | NumberToken n ->
        Printf.fprintf out_stream "NumberToken %i\n" n;   (* This is kinda gross but apparently fprintf doesn't like currying /shrug *)
        write_token (next_tok ())
    | OpenParenToken ->
        print_out "OpenParenToken\n";
        write_token (next_tok ())
    | SymbolToken sym ->
        Printf.fprintf out_stream "SymbolToken %s\n" sym;
        write_token (next_tok ())
  in
  write_token (next_tok ())   (* Start the recursion *)
in

(*
 * Makes a parser and writes its output to the given file
 *)
let execute_and_write (file_in : string) (file_out : string) : unit =
  Printf.printf "Beginning parsing of %s..." file_in;
  let out_stream = open_out file_out in   (* Open the file to write your results to *)
  let next_tok = makeScanner file_in in   (* Make a scanner from the input file *)
  write_tokens (next_tok) out_stream;    (* Write everything recursively *)
  flush out_stream;
  Printf.printf "\n  Written to %s!\n" file_out
in

(*
 * This is what actually executes!
 * Wrapper function for all this stuff so it's not top-level
 *)
let main () =
  let test_files = Array.to_list (Sys.readdir test_cases_directory) in
  let test_files_paths = List.map (fun name -> Filename.concat test_cases_directory name) test_files in
  Printf.printf "Test cases found:\n";
  List.iter (Printf.printf "    + %s\n") test_files;

  let out_files = List.map (fun name -> (Filename.chop_extension name) ^ ".out") test_files in
  let out_files_paths = List.map (fun name -> Filename.concat your_output_directory name) out_files in
  Printf.printf "Your output files will be:\n";
  List.iter (Printf.printf "    - %s\n") out_files;
  Printf.printf "   in %s\n\n" your_output_directory;
  Printf.printf "Beginning the run-through of all files...\n\n";

  List.iter2 (execute_and_write) test_files_paths out_files_paths;

  Printf.printf "\nAll tests finished!\n";

  (* If we're on Unix, we can use the diff command to figure stuff out *)
  if run_diff && Sys.os_type = "Unix" then begin
    Printf.printf "Running diff on ouput...\n\n";
    flush stdout;
    List.iter 
      (fun name ->
        let your_output = Filename.concat your_output_directory name in  (* Get the filename of your output *)
        let expected_output = Filename.concat expected_output_directory name in  (* Get the filename of our output *)
        let diff = Printf.sprintf "diff -q %s %s" your_output expected_output in  (* Run diff on them (quiet mode) *)
        let code = Sys.command diff in
        match code with
        | 0 -> Printf.printf "No differences between %s and %s\n" your_output expected_output
        | 1 -> Printf.printf "%s is different from %s!\n" your_output expected_output
        | _ -> () )
      out_files;
    Printf.printf "\nTesting script finished!\n";
  end
in
main ();;

