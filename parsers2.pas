unit parsers2;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, contnrs;

type
  TParseResult = record
    Success: Boolean;
    Position: Integer;
    Result: String;
  end;

  { TParser }

  TParser = class
    function Run(Input: String; Position: Integer): TParseResult; virtual; abstract;
  end;

  { TLitParser }

  TLitParser = class(TParser)
    FLit: String;
    constructor Create(Lit: String);
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;

  { TOrParser }

  TOrParser = class(TParser)
    FA, FB: String;
    constructor Create(A, B: String);
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;

  { TAndParser }

  TAndParser = class(TParser)
    FA, FB: String;
    constructor Create(A, B: String);
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;


  procedure AddParser(Name: String; Parser: TParser);
  procedure AddParser(Name: String; Parser: String);
  function GetParser(Name: String): TParser;

  operator or(A, B: String): String;
  operator and(A, B: String): String;

  function Lit(S: String): String;

var
  Parsers: TFPHashObjectList;

implementation
var
  Count: Integer = 0;

function UniqueName: String;
begin
  Result := IntToHex(Count, 8);
  Inc(Count);
end;

operator or(A, B: String): String;
begin
  Result := UniqueName;
  Parsers.Add(Result, TOrParser.Create(A, B));
end;

operator and(A, B: String): String;
begin
  Result := UniqueName;
  Parsers.Add(Result, TAndParser.Create(A, B));
end;

function Lit(S: String): String;
begin
  Result := UniqueName;
  Parsers.Add(Result, TLitParser.Create(S));
end;

{ TAndParser }

constructor TAndParser.Create(A, B: String);
begin
  FA := A;
  FB := B;
end;

function TAndParser.Run(Input: String; Position: Integer): TParseResult;
var
  RA, RB: TParseResult;
begin
  Result.Success := False;
  Result.Position := Position;
  RA := GetParser(FA).Run(Input, Position);
  if RA.Success then begin
    RB := GetParser(FB).Run(Input, RA.Position);
    if RB.Success then begin
      Result.Success := True;
      Result.Position := RB.Position;
      Result.Result := RA.Result + RB.Result;
    end;
  end;
end;

{ TOrParser }

constructor TOrParser.Create(A, B: String);
begin
  FA := A;
  FB := B;
end;

function TOrParser.Run(Input: String; Position: Integer): TParseResult;
begin
  Result := GetParser(FA).Run(Input, Position);
  if not Result.Success then
    Result := GetParser(FB).Run(Input, Position);
end;

{ TLitParser }

constructor TLitParser.Create(Lit: String);
begin
  FLit := Lit;
end;

function TLitParser.Run(Input: String; Position: Integer): TParseResult;
begin
  Result.Success := False;
  Result.Position := Position;
  if Position + Length(FLit) <= Length(Input) + 1 then begin
    if Copy(Input, Position, Length(FLit)) = FLit then begin
      Result.Success := True;
      Result.Position := Position + Length(FLit);
      Result.Result := FLit;
    end;
  end;
end;


procedure AddParser(Name: String; Parser: TParser);
begin
  Parsers.Add(Name, Parser);
end;

procedure AddParser(Name: String; Parser: String);
begin
  Parsers.Rename(Parser, Name);
end;

function GetParser(Name: String): TParser;
begin
  Result := TParser(Parsers.Find(Name));
end;

initialization
  Parsers := TFPHashObjectList.Create;
finalization
  Parsers.Free;
end.

