program parsecomb;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, parsers2;

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
  I: Integer;
  EXPR, PARENS, INNER, MULFUNC, ADDFUNC: TParser;
  _PARENS: TParserForwardDeclaration;

begin
  // there is recursion in the grammar, so
  // we need a placeholder (kind of like a
  // forward declaration)
  _PARENS := TParserForwardDeclaration.Create;

  // define the grammar by combining many smaller
  // simple parsers to one big complex parser
  EXPR      := Num or _PARENS;
  MULFUNC   := Sym('mul') and EXPR and EXPR;
  ADDFUNC   := Sym('add') and EXPR and EXPR;
  INNER     := MULFUNC or ADDFUNC or Num;
  PARENS    := Sym('(') and INNER and Sym(')');

  // now solve the forward declaration, close the circle
  _PARENS.Impl := PARENS;

  // some of the above parsers need to perform
  // some action, otherwise it would be boring.
  EXPR.PostProc := @ProcExpr;
  ADDFUNC.PostProc := @ProcAdd;
  MULFUNC.PostProc := @ProcMul;

  // time: 1.60s
  for I := 1 to 100000 do begin
    EXPR.Run('(mul (add (add 200 2) (mul 2 2)) 34)', 1);
  end;

  Print(EXPR.Run('(mul (add (add 200 2) (mul 2 2)) 34)', 1));
end.

