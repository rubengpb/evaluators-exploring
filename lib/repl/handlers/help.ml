let handle_help st =
   print_endline
     ("Simple λ-REPL: type lambda-terms, assign lambda-terms" ^
     " and use them. \n\nType :q for exit." ^
      "\nType :h for help." ^
      "\nType :info to know the current evaluator." ^
      "\nType :set <eval> to change the current evaluator." ^
      "\nType :env to see the current definitions." ^
      "\nType <var> = <term> to assing a varible to a term." ^
      "\nType <term> to evaluate a term." ^
    "\nType :t <id> to know the type.");
   st

