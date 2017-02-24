unit parsers2;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, contnrs;

type
  TParseResult = record
    Success: Boolean;
    Position: Integer;
    Result: TStringArray;
  end;

  TPostProc = procedure(var Result: TParseResult);
  TPostMeth = procedure(var Result: TParseResult) of object;

  { TParser }

  TParser = class
    PostProc: TPostProc;
    PostMeth: TPostMeth;
    function Run(Input: String; Position: Integer): TParseResult; virtual; abstract;
    procedure RunPostProc(var Result: TParseResult);
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

function StringArray(A: String): TStringArray;
begin
  SetLength(Result, 1);
  Result[0] := A;
end;

function StringArray(A, B: TStringArray): TStringArray;
var
  I: Integer;
begin
  SetLength(Result, Length(A) + Length(B));
  for I := 0 to Length(A) - 1 do
    Result[I] := A[I];
  for I := 0 to Length(B) - 1 do
    Result[Length(A) + I] := B[I];
end;

{ TParser }

procedure TParser.RunPostProc(var Result: TParseResult);
begin
  if Assigned(PostProc) then PostProc(Result);
  if Assigned(PostMeth) then PostMeth(Result);
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
      Result.Result := StringArray(RA.Result, RB.Result);
      RunPostProc(Result);
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
  if Result.Success then
    RunPostProc(Result);
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
      Result.Result := StringArray(FLit);
      RunPostProc(Result);
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

