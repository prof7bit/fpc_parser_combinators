{ experimantal parser combinator demo
  with runnables instead of lambdas to see
  how far object pascal can be pushed

  dedicated to the public domain,
  prof7bit@gmail.com
}
program parsecomb;

{$mode objfpc}{$H+}{$assertions on}

uses
  //heaptrc,
  Classes, sysutils, parsercombinators;

procedure ProcExpr(var R: TParseResult);
begin
  if R.Data.Typ = ntBranch then begin
    //DebugPrint(R);
    { The structure of this Node is
    ntBranch:
        ntBranch:
            ntString: (
            ntInt: 5
        ntString: )

    replace this entire ntBranch with only the Node
    found between the two parenthesis }
    R.Data := R.Data.Child[0].Child[1];
  end;
end;

procedure ProcAdd(var R: TParseResult);
begin
  //DebugPrint(R);
  { the structure of this Node is:
  ntBranch:
      ntBranch:
          ntString: add
          ntInt: 2
      ntInt: 3

  replace this entire ntBranch with a single
  ntInt leaf containing only the calculated result }
  R.Data.Int := R.Data.Child[0].Child[1].Int + R.Data.Child[1].Int;
  R.Data.Typ := ntInt;
  SetLength(R.Data.Child, 0);
end;

procedure ProcMul(var R: TParseResult);
begin
  //DebugPrint(R);
  { the structure of this Node is:
  ntBranch:
      ntBranch:
          ntString: mul
          ntInt: 2
      ntInt: 3

  replace this entire ntBranch with a single
  ntInt leaf containing only the calculated result }
  R.Data.Int := R.Data.Child[0].Child[1].Int * R.Data.Child[1].Int;
  R.Data.Typ := ntInt;
  SetLength(R.Data.Child, 0);
end;

var
  A: String;
  R: TParseResult;
  EXPR, PARENS, INNER, MULFUNC, ADDFUNC, _PARENS: TParser;

begin
  // there is recursion in the grammar, so we need a
  // placeholder (kind of like a forward declaration)
  _PARENS := TParser.Create;

  // define the grammar
  EXPR      := Num or _PARENS;
  MULFUNC   := Sym('mul') and EXPR and EXPR;
  ADDFUNC   := Sym('add') and EXPR and EXPR;
  INNER     := MULFUNC or ADDFUNC or Expr;
  PARENS    := Sym('(') and INNER and Sym(')');

  // solve the forward declaration, closing the circle
  _PARENS.SetImplementation(PARENS);

  // some of the above parsers need to perform
  // some action, otherwise it would be boring.

  EXPR.SetPostProc(@ProcExpr);
  ADDFUNC.SetPostProc(@ProcAdd);
  MULFUNC.SetPostProc(@ProcMul);

  R := EXPR.Run('(mul (add (add 200 2) (mul 2 2)) 34)', 1);
  DebugPrint(R.Data);
  Assert(R.Success);
  Assert(R.Data.Typ = ntInt);
  Assert(R.Data.Int = 7004);

  repeat
    WriteLn('Type something that evals to 42 to quit');
    ReadLn(A);
    R := EXPR.Run(A, 1);
    DebugPrint(R.Data);
  until R.Success and (R.Data.Typ = ntInt) and (R.Data.Int = 42);

  // freeing the root of the parser tree will free all
  // contained parsers recursively, even in the presence
  // of cyclic references, heaptrc should indicate no
  // leaks anymore after the call below
  EXPR.Free;
end.

