{ experimantal parser combinator demo
  with runnables instead of lambdas to see
  how far object pascal can be pushed

  dedicated to the public domain,
  prof7bit@gmail.com
}
program parsecomb;

{$mode objfpc}{$H+}

uses
  //heaptrc,
  Classes, sysutils, parsercombinators;

procedure Print(Res: TParseResult);
var
  S: String;
begin
  WriteLn('Success: ', Res.Success);
  WriteLn('Position: ',  Res.Position);
  WriteLn('---------------');
  for S in Res.Result do
    WriteLn(S);
end;

procedure ProcExpr(var R: TParseResult);
begin
  if Length(R.Result) = 3 then begin
    // the array is [ '(', value, ')' ]
    // remove parens and shrink array
    R.Result[0] := R.Result[1];
    SetLength(R.Result, 1);
  end;
end;

procedure ProcAdd(var R: TParseResult);
begin
  // the array is [ 'add', a, b ]
  // replace it with an array
  // containing only the sum
  R.Result[0] := IntToStr(StrToInt(R.Result[1]) + StrToInt(R.Result[2]));
  SetLength(R.Result, 1);
end;

procedure ProcMul(var R: TParseResult);
begin
  // the array is [ 'mul', a, b ]
  // replace it with an array
  // containing only the product
  R.Result[0] := IntToStr(StrToInt(R.Result[1]) * StrToInt(R.Result[2]));
  SetLength(R.Result, 1);
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
  INNER     := MULFUNC or ADDFUNC or Num;
  PARENS    := Sym('(') and INNER and Sym(')');

  // solve the forward declaration, closing the circle
  _PARENS.SetImplementation(PARENS);

  // some of the above parsers need to perform
  // some action, otherwise it would be boring.
  EXPR.SetPostProc(@ProcExpr);
  ADDFUNC.SetPostProc(@ProcAdd);
  MULFUNC.SetPostProc(@ProcMul);

  R := EXPR.Run('(mul (add (add 200 2) (mul 2 2)) 34)', 1);
  Assert(R.Success);
  Assert(Length(R.Result) = 1);
  Assert(R.Result[0] = '7004');

  repeat
    WriteLn('Type something that evals to 42 to quit');
    ReadLn(A);
    R := EXPR.Run(A, 1);
    Print(R);
  until R.Success and (R.Result[0] = '42');

  // freeing the root of the parser tree will free all
  // contained parsers recursively, even in the presence
  // of cyclic references, heaptrc should indicate no
  // leaks anymore after the call below
  EXPR.Free;
end.

