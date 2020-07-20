type id =
  | Global(string)
  | ExplicitGlobal(string)
  | ScriptLocal(string)
  | BufferLocal(string)
  | WindowLocal(string)
  | Predefined(string)
  | Param(string)
  | Environment(string)
  | Opt(string)
  | Register(string);

type assign_op =
  | Eq
  | PlusEq
  | MinusEq
  | TimesEq
  | DivEq;

type bin_op =
  | EqEq
  | NotEq
  | Less
  | LessEq
  | Great
  | GreatEq
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Concat
  | Match
  | NotMatch;

type un_op =
  | Not;

type expression =
  | Num(int)
  | Str(string)
  | Var(id)
  | Binary(bin_op, expression, expression)
  | Unary(un_op, expression)
  | Ternary(expression, expression, expression)
  | Invoke(id, list(expression))
  | List(list(expression))
  | Dictionary(list((string, expression)))
  | DictAccess(expression, expression)
  | DotAccess(expression, string)

and function_id =
  | GlobalFuncId(string)
  | ScriptLocalFuncId(string)
  | DictFuncId(expression, string);

type statement =
  | Let(id, assign_op, expression)
  | Unlet(id, bool)
  | Set(string)
  | While(expression, list(statement))
  | For(id, expression, list(statement))
  | If(
      expression,
      list(statement),
      list((expression, list(statement))),
      option(list(statement)),
    )
  | Echo(list(expression))
  | Call(expression, list(expression))
  | Function(function_id, bool, list(string), bool, bool, list(statement))
  | DelFunction(function_id)
  | Return(option(expression))
  | TryCatch(
      list(statement),
      list((option(string), list(statement))),
      option(list(statement)),
    )
  | Finish;
