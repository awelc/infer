open Go_ast_to_json_t

  let load_file f = (
	let ic = open_in f in
	let n = in_channel_length ic in
(*	let s = String.create n in
	really_input ic s 0 n; *)
     let s = really_input_string ic n in
	 close_in ic; s)

let () =
	let json = (load_file "/Users/adam.welc/tmp.json") in
    print_endline (Go_ast_to_json_j.string_of_file_type (Go_ast_to_json_j.file_type_of_string json))
