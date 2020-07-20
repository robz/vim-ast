open Vimscript;

let statements = [
  Let(Global("j"), Eq, Num(1)),
  While(
    Binary(Less, Var(Global("i")), Num(5)),
    [
      Echo([Str("count is"), Var(Global("i"))]),
      Let(Global("i"), PlusEq, Num(1)),
    ],
  ),
  For(
    Global("i"),
    Invoke(Global("range"), [Num(1), Num(4)]),
    [Echo([Str("count is"), Var(Global("i"))])],
  ),
];

print_endline(String_of_vimscript.string_of_statements("", statements));
