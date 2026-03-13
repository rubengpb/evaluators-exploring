let main_message =
  "Simple λ-REPL: type lambda-terms, assign lambda-terms" ^
  " and use them in expressions. \n\nType :q to exit.\n\n" ^
  "   Commands:\n" ^
  "     :load <file>\n" ^
  "     :t <id>|<term>\n" ^
  "     :config <param>\n" ^
  "     :set <param> <opt>\n\n" ^
  "More info about each command with :h <command>.\n\n" ^
  "   Evaluation of terms:\n" ^
  "     Type <id> = <term> to assign to an id a term.\n" ^
  "     Type <term> to evaluate a term."

let handle_help st = function
  | None ->
   print_endline main_message;
   st
  | Some s ->
    print_endline
    (match s with
    | "q" -> "Type :q to exit"
    | "h" -> main_message
    | "load" -> "Type :load <file> to load predifined definitions.\n\n" ^
        "If the file depends on another file, write at the first line: " ^
        "include <other-file>+."
    | "t" -> "Type :t <id> or :t <term> in order to see the type of the term."
    | "config" -> "Config options:\n\n" ^
          "   :config eval  (displays the current evaluator)\n" ^
          "   :config env  (displays the current environment, with the map of <id> = <term>)\n" ^
          "   :config church  (format numbers in the output, true or false)\n" ^
          "   :config display  (print the initial term before evaluation, true or false)"
    | "set" -> "Set options:\n\n" ^
          "   :set eval <eval>  (set the current evaluator to <eval>)\n" ^
          "   :set env clear  (clear the environment)\n" ^
          "   :set church (true|false)  (activate/deactivate church output)\n" ^
          "   :set display (true|false) (activate/deactivate initial print)"
    | other -> "[ERROR in help]\nNon exists this instruction: " ^ other);
    st

