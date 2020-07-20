open Vimscript;

let string_of_id =
  fun
  | Global(name) => name
  | ExplicitGlobal(name) => "g:" ++ name
  | ScriptLocal(name) => "s:" ++ name
  | BufferLocal(name) => "b:" ++ name
  | WindowLocal(name) => "w:" ++ name
  | Param(name) => "a:" ++ name
  | Predefined(name) => "v:" ++ name
  | Environment(name) => "$" ++ name
  | Opt(name) => "&" ++ name
  | Register(name) => "@" ++ name;

let string_of_assign_op =
  fun
  | Eq => "="
  | PlusEq => "+="
  | MinusEq => "-="
  | TimesEq => "*="
  | DivEq => "/=";

let string_of_binop =
  fun
  | EqEq => "=="
  | NotEq => "!="
  | Less => "<"
  | LessEq => "<="
  | Great => ">"
  | GreatEq => ">="
  | Add => "+"
  | Sub => "-"
  | Mult => "*"
  | Div => "/"
  | Mod => "%"
  | Concat => "."
  | Match => "=~"
  | NotMatch => "!~";

let string_of_unop =
  fun
  | Not => "!";

let rec string_of_expression =
  fun
  | Num(i) => string_of_int(i)
  | Str(s) => "\"" ++ s ++ "\""
  | Var(id) => string_of_id(id)
  | Binary(binop, e1, e2) =>
    string_of_expression(e1)
    ++ " "
    ++ string_of_binop(binop)
    ++ " "
    ++ string_of_expression(e2)
  | Unary(unop, e) => string_of_unop(unop) ++ " " ++ string_of_expression(e)
  | Ternary(e1, e2, e3) =>
    string_of_expression(e1)
    ++ " ? "
    ++ string_of_expression(e2)
    ++ " : "
    ++ string_of_expression(e3)
  | Invoke(id, args) =>
    string_of_id(id)
    ++ "("
    ++ String.concat(", ", List.map(string_of_expression, args))
    ++ ")"
  | List(elements) =>
    "["
    ++ String.concat(", ", List.map(string_of_expression, elements))
    ++ "]"
  | Dictionary(keyvals) =>
    "{"
    ++ String.concat(
         ", ",
         List.map(
           ((k, v)) => "\"" ++ k ++ "\": " ++ string_of_expression(v),
           keyvals,
         ),
       )
    ++ "}"
  | DictAccess(e1, e2) =>
    string_of_expression(e1) ++ "[" ++ string_of_expression(e2) ++ "]"
  | DotAccess(e, key) => string_of_expression(e) ++ "." ++ key

and string_of_function_id =
  fun
  | GlobalFuncId(name) => name
  | ScriptLocalFuncId(name) => "s:" ++ name
  | DictFuncId(e, name) => string_of_expression(e) ++ "." ++ name;

let rec string_of_statement = tab =>
  fun
  | Let(id, op, e) =>
    tab
    ++ "let "
    ++ string_of_id(id)
    ++ " "
    ++ string_of_assign_op(op)
    ++ " "
    ++ string_of_expression(e)
  | Unlet(id, force) =>
    tab ++ (force ? "!" : "") ++ "unlet " ++ string_of_id(id)
  | Set(name) => tab ++ "set " ++ name
  | While(condition, statements) =>
    tab
    ++ "while "
    ++ string_of_expression(condition)
    ++ "\n"
    ++ string_of_statements(tab ++ "  ", statements)
    ++ "\n"
    ++ tab
    ++ "endwhile"
  | For(id, expression, statements) =>
    tab
    ++ "for "
    ++ string_of_id(id)
    ++ " in "
    ++ string_of_expression(expression)
    ++ "\n"
    ++ string_of_statements(tab ++ "  ", statements)
    ++ "\n"
    ++ tab
    ++ "endfor"
  | If(condition, consequent, elseifs, els) =>
    tab
    ++ "if "
    ++ string_of_expression(condition)
    ++ "\n"
    ++ string_of_statements(tab ++ "  ", consequent)
    ++ String.concat(
         "",
         List.map(
           ((condition, statements)) =>
             tab
             ++ "\nelseif "
             ++ string_of_expression(condition)
             ++ "\n"
             ++ string_of_statements(tab ++ "  ", statements),
           elseifs,
         ),
       )
    ++ (
      switch (els) {
      | None => ""
      | Some(statements) =>
        tab ++ "\nelse\n" ++ string_of_statements(tab ++ "  ", statements)
      }
    )
    ++ tab
    ++ "endif"
  | Echo(expressions) =>
    tab
    ++ "echo "
    ++ String.concat(" ", List.map(string_of_expression, expressions))
  | Call(e, args) =>
    tab
    ++ string_of_expression(e)
    ++ "("
    ++ String.concat(", ", List.map(string_of_expression, args))
    ++ ")"
  | Function(id, redefine, params, spread, range, statements) =>
    tab
    ++ "function\n"
    ++ (redefine ? "! " : " ")
    ++ string_of_function_id(id)
    ++ "("
    ++ String.concat(", ", params)
    ++ (spread ? ", ..." : "")
    ++ ")"
    ++ (range ? " range" : "")
    ++ (
      switch (id) {
      | DictFuncId(_) => " dict"
      | _ => ""
      }
    )
    ++ "\n"
    ++ string_of_statements(tab ++ "  ", statements)
    ++ tab
    ++ "endfunction"
  | DelFunction(id) => tab ++ "delfunction " ++ string_of_function_id(id)
  | Return(e) =>
    tab
    ++ "return"
    ++ (
      switch (e) {
      | None => ""
      | Some(e) => " " ++ string_of_expression(e)
      }
    )
  | TryCatch(tries, catches, finally) =>
    tab
    ++ "try\n"
    ++ string_of_statements(tab ++ "  ", tries)
    ++ String.concat(
         "",
         List.map(
           ((pattern, statements)) =>
             tab
             ++ "\ncatch"
             ++ (
               switch (pattern) {
               | None => ""
               | Some(pattern) => " /" ++ pattern ++ "/"
               }
             )
             ++ "\n"
             ++ string_of_statements(tab ++ "  ", statements),
           catches,
         ),
       )
    ++ (
      switch (finally) {
      | None => ""
      | Some(finally) =>
        tab
        ++ "\nfinally\n"
        ++ string_of_statements(tab ++ "  ", finally)
        ++ "\n"
      }
    )
    ++ tab
    ++ "endtry"
  | Finish => tab ++ "finish"

and string_of_statements = (tab, statements) =>
  String.concat("\n", List.map(string_of_statement(tab), statements));
