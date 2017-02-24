unit parsers2;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, contnrs;

type
  TParseResult = record
    Success: Boolean;
    Position: Integer;
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

  { TParsers }

  TParsers = class
    FParsers: TFPHashObjectList;
    constructor Create;
    destructor Destroy; override;
    procedure Add(Name: String; Parser: TParser);
    procedure Add(Name: String; Parser: String);
    function Get(Name: String): TParser;
  end;

  operator or(A, B: String): String;
  operator and(A, B: String): String;

  function Lit(S: String): String;

var
  Parsers: TParsers;

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
  RA := Parsers.Get(FA).Run(Input, Position);
  if RA.Success then begin
    RB := Parsers.Get(FB).Run(Input, RA.Position);
    if RB.Success then begin
      Result.Success := True;
      Result.Position := RB.Position;
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
  Result := Parsers.Get(FA).Run(Input, Position);
  if not Result.Success then
    Result := Parsers.Get(FB).Run(Input, Position);
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
    end;
  end;
end;

{ TParsers
  Store and retieve parsers by name}

constructor TParsers.Create;
begin
  FParsers := TFPHashObjectList.Create;
end;

destructor TParsers.Destroy;
begin
  FParsers.Free;
  inherited Destroy;
end;

procedure TParsers.Add(Name: String; Parser: TParser);
begin
  FParsers.Add(Name, Parser);
end;

procedure TParsers.Add(Name: String; Parser: String);
begin
  FParsers.Rename(Parser, Name);
end;

function TParsers.Get(Name: String): TParser;
begin
  Result := TParser(FParsers.Find(Name));
end;

initialization
  Parsers := TParsers.Create;
finalization
  Parsers.Free;
end.

