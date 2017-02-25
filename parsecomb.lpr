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
  Classes, sysutils, parsercombinators, variants;


procedure Print(V: Variant; I: Integer);
var
  J: Integer;
begin
  if VarIsArray(V) then begin
    for J := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do begin
      Print(V[J], I + 4);
    end;
  end
  else begin
    for J := 0 to I do
      Write(' ');
    WriteLn(V);
  end;
end;

procedure Print(Res: TParseResult);
begin
  WriteLn('Success: ', Res.Success);
  WriteLn('Position: ',  Res.Position);
  WriteLn('---------------');
  Print(Res.Result, 0);
end;

procedure ProcExpr(var R: TParseResult);
begin
  if VarIsArray(R.Result) then begin
    // the array is [ '(', value, ')' ]
    // replace it by the value only
    R.Result := R.Result[1];
  end;
end;

procedure ProcAdd(var R: TParseResult);
begin
  // the array is [ 'add', a, b ]
  // replace it with the sum a+b
  R.Result := R.Result[1] + R.Result[2];
end;

procedure ProcMul(var R: TParseResult);
begin
  // the array is [ 'mul', a, b ]
  // replace it with the product a*b
  R.Result := R.Result[1] * R.Result[2];
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
  Print(R);

  repeat
    WriteLn('Type something that evals to 42 to quit');
    ReadLn(A);
    R := EXPR.Run(A, 1);
    Print(R);
  until R.Success and (R.Result = 42);

  // freeing the root of the parser tree will free all
  // contained parsers recursively, even in the presence
  // of cyclic references, heaptrc should indicate no
  // leaks anymore after the call below
  EXPR.Free;
end.

