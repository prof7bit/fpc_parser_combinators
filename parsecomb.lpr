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
  Parsers.Add('expr', Lit('(') and 'inner' and Lit(')'));
  Parsers.Add('inner', Lit('bar') or 'expr');


  Print(Parsers.Get('expr').Run('((bar))', 1));

end;

begin
  Main;
end.

