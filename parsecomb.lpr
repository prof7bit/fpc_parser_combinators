program parsecomb;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, parsers2;

procedure Print(Res: TParseResult);
begin
  WriteLn(Res.Success);
  WriteLn(Res.Position);
end;

procedure Main;

begin
  Parsers.Add('expr',    Lit('(') and 'inner' and Lit(')'));
  Parsers.Add('inner',   'oper' and 'arg' and 'arg');
  Parsers.Add('oper',    Lit('add') or Lit('mul') or 'expr');
  Parsers.Add('arg',     Lit(' ') and ('digit' or 'expr'));
  Parsers.Add('digit',   Lit('0') or Lit('1') or Lit('2') or Lit('3'));

  Print(Parsers.Get('expr').Run('(mul (add 2 3) 2)', 1));
end;

begin
  Main;
end.

