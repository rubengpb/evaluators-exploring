let prompt = "λ> "

let rec loop () =
  print_string prompt;
  flush stdout;
  match read_line () with
  | exception End_of_file ->
      print_endline "\nBye!";
      ()
  | line ->
      print_endline line;
      print_endline line;
      loop ()
