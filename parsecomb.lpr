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

procedure PostProcExpr(var Result: TParseResult);
var
  A, B: Integer;
begin
  A := StrToInt(Result.Result[3]);
  B := StrToInt(Result.Result[5]);
  if Result.Result[1] = 'add' then begin
    WriteLn('processing addition');
    SetLength(Result.Result, 1);
    Result.Result[0] := IntToStr(A + B);
  end
  else if Result.Result[1] = 'mul' then begin
    WriteLn('processing multiplication');
    SetLength(Result.Result, 1);
    Result.Result[0] := IntToStr(A * B);
  end;
end;

procedure Main;

begin
  AddParser('expr',    Lit('(') and 'inner' and Lit(')'));
  AddParser('inner',   'oper' and 'arg' and 'arg');
  AddParser('oper',    Lit('add') or Lit('mul') or 'expr');
  AddParser('arg',     Lit(' ') and ('digit' or 'expr'));
  AddParser('digit',   Lit('0') or Lit('1') or Lit('2') or Lit('3'));

  GetParser('expr').PostProc := @PostProcExpr;

  Print(GetParser('expr').Run('(mul (add (add 2 2) (mul 2 2)) 3)', 1));
end;

begin
  Main;
end.

