let main_message =
  "Simple λ-REPL: type lambda-terms, assign lambda-terms" ^
  " and use them. \n\nType :q for exit.\n\n" ^
  "   Commands:\n" ^
  "     :load <file>\n" ^
  "     :t <id>|<term>\n" ^
  "     :config <param>\n" ^
  "     :set <param>\n\n" ^
  "More info about each command with :h <command>.\n\n" ^
  "   Evaluation of terms:\n" ^
  "     Type <id> = <term> to assing to an id a term.\n" ^
  "     Type <term> to evaluate a term."

let handle_help st = function
  | None ->
   print_endline main_message;
   st
  | Some s ->
    print_endline
    (match s with
    | "q" -> "Type :q for exit"
    | "h" -> main_message
    | "load" -> "Type :load <file> to charge prebuilt-in definitions.\n\n" ^
        "If the file has dependencies of other file, write at the first line: " ^
        "include <other-file>+."
    | "t" -> "Type :t <id> or :t <term> in order to know the type of the term."
    | "config" -> "Options of config:\n\n" ^
          "   :config eval  (displais the current evaluator)\n" ^
          "   :config env  (displais the current evairoment, with the map of <id> = <term>)\n" ^
          "   :config church  (format numbers in the output. ON or OFF)\n" ^
          "   :config display  (print the initial term before evaluating. ON or OFF)"
    | "set" -> "Options of set:\n\n" ^
          "   :set eval <eval>  (set <eval> at current evaluator)\n" ^
          "   :set env clean  (clean the enviroment)\n" ^
          "   :set church (on|off)  (activate/disactivate church output)\n" ^
          "   :config display (on|off) (activate/disactivate initial print)"
    | other -> "[ERROR in help]\nNon exists this instruction: " ^ other);
    st

