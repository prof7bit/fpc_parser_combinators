program parsecomb;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, parsers;


procedure Main;
var
  P: IParser;
begin
  P := (Symbol('(') and (Symbol('add') or Symbol('mul')) and Symbol(')'));
  P.Run(TBytes('(mul)'), 0).Print;
end;

begin
  Main;
end.

