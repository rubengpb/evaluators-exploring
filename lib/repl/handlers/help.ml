let read_file path =
  try
    let ic = open_in path in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    content
  with _ -> "[ERROR] Check that the documentations files have been uploaded."

let help_path cmd = "help/" ^ cmd ^ ".txt"

let handle_help st = function
  | None ->
    let path = help_path "main" in
    print_endline (read_file path);
    st
  | Some s ->
    print_endline (
      match s with
      | "q" | "quit" -> read_file @@ help_path "q"
      | "h" | "help" -> read_file @@ help_path "main"
      | "load" as cmd -> read_file @@ help_path cmd
      | "t" | "type" -> read_file @@ help_path "type"
      | "config" | "iconfig" -> read_file @@ help_path "config"
      | "set" as cmd -> read_file @@ help_path cmd
      | "evals" as cmd -> read_file @@ help_path cmd
      | "names" as cmd -> read_file @@ help_path cmd
      | "env" as cmd -> read_file @@ help_path cmd
      | "church" | "ch" -> read_file @@ help_path "church"
      | "prebuilt" as cmd -> read_file @@ help_path cmd
      | _ -> "[ERROR in help] Command not found: " ^ s
      );
    st
