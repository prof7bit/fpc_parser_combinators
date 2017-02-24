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

begin
  AddParser('EXPR',     Num or 'PARENS');
  AddParser('PARENS',   Sym('(') and 'INNER' and Sym(')'));
  AddParser('INNER',    'MULFUNC' or 'ADDFUNC' or Num);
  AddParser('MULFUNC',  Sym('mul') and 'EXPR' and 'EXPR');
  AddParser('ADDFUNC',  Sym('add') and 'EXPR' and 'EXPR');

  GetParser('EXPR').PostProc := @ProcExpr;
  GetParser('ADDFUNC').PostProc := @ProcAdd;
  GetParser('MULFUNC').PostProc := @ProcMul;

  // time: 2.48s
  for I := 1 to 100000 do begin
    GetParser('EXPR').Run('(mul (add (add 200 2) (mul 2 2)) 34)', 1);
  end;

  Print(GetParser('EXPR').Run('(mul (add (add 200 2) (mul 2 2)) 34)', 1));
end.

