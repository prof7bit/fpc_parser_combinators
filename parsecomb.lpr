program parsecomb;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, parsers2;

procedure Print(Res: TParseResult);
begin
  WriteLn(Res.Success);
  WriteLn(Res.Position);
  WriteLn(Res.Result);
end;

procedure Main;

begin
  AddParser('expr',    Lit('(') and 'inner' and Lit(')'));
  AddParser('inner',   'oper' and 'arg' and 'arg');
  AddParser('oper',    Lit('add') or Lit('mul') or 'expr');
  AddParser('arg',     Lit(' ') and ('digit' or 'expr'));
  AddParser('digit',   Lit('0') or Lit('1') or Lit('2') or Lit('3'));

  Print(GetParser('expr').Run('(mul (add 2 3) 2)', 1));
end;

begin
  Main;
end.

