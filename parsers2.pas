unit parsers2;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils;

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

  { TParserForwardDeclaration }

  TParserForwardDeclaration = class(TParser)
    Impl: TParser;
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;

  { TLitParser }

  TLitParser = class(TParser)
    FLit: String;
    constructor Create(Lit: String);
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;

  { TWhitespaceParser }

  TWhitespaceParser = class(TParser)
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;

  { TNumberParser }

  TNumberParser = class(TParser)
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;

  { TDigitParser }

  TDigitParser = class(TParser)
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;

  { TOrParser }

  TOrParser = class(TParser)
    FA, FB: TParser;
    constructor Create(A, B: TParser);
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;

  { TAndParser }

  TAndParser = class(TParser)
    FA, FB: TParser;
    constructor Create(A, B: TParser);
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;


  operator or(A, B: TParser): TParser;
  operator and(A, B: TParser): TParser;

  function Lit(S: String): TParser;
  function White: TParser;
  function Sym(S: String): TParser;
  function Num: TParser;

implementation

operator or(A, B: TParser): TParser;
begin
  Result := TOrParser.Create(A, B);
end;

operator and(A, B: TParser): TParser;
begin
  Result := TAndParser.Create(A, B);
end;

function Lit(S: String): TParser;
begin
  Result := TLitParser.Create(S);
end;

function White: TParser;
begin
  Result := TWhitespaceParser.Create;
end;

function Sym(S: String): TParser;
begin
  Result := White and Lit(S);
end;

function Num: TParser;
begin
  Result := White and TNumberParser.Create;
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

{ TParserForwardDeclaration
  use this as a placeholder parser if the grammar has recursive references,
  you can then later assign to Impl a reference to the actual parser object }

function TParserForwardDeclaration.Run(Input: String; Position: Integer): TParseResult;
begin
  if not Assigned(Impl) then
    raise Exception.Create('placeholder-parser is empty');
  Result := Impl.Run(Input, Position);
  RunPostProc(Result);
end;

{ TNumberParser }

function TNumberParser.Run(Input: String; Position: Integer): TParseResult;
begin
  Result.Position := Position;
  Result.Success := False;
  repeat
    if not (Input[Result.Position] in ['0'..'9']) then
      break;
    Inc(Result.Position);
    Result.Success := True;
  until Result.Position > Length(Input);
  if Result.Success then begin
    Result.Result := StringArray(Copy(Input, Position, Result.Position - Position));
    RunPostProc(Result);
  end;
end;


{ TDigitParser }

function TDigitParser.Run(Input: String; Position: Integer): TParseResult;
begin
  if Input[Position] in ['0'..'9'] then begin
    Result.Success := True;
    Result.Position := Position + 1;
    Result.Result := StringArray(Input[Position]);
  end;
end;

{ TWhitespaceParser }

function TWhitespaceParser.Run(Input: String; Position: Integer): TParseResult;
begin
  Result.Position := Position;
  repeat
    if not (Input[Result.Position] in [' ', #9, #10, #13]) then
      break;
    Inc(Result.Position);
  until Result.Position > Length(Input);
  Result.Success := True;
  RunPostProc(Result);
end;

{ TParser }

procedure TParser.RunPostProc(var Result: TParseResult);
begin
  if Assigned(PostProc) then PostProc(Result);
  if Assigned(PostMeth) then PostMeth(Result);
end;

{ TAndParser }

constructor TAndParser.Create(A, B: TParser);
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
  RA := FA.Run(Input, Position);
  if RA.Success then begin
    RB := FB.Run(Input, RA.Position);
    if RB.Success then begin
      Result.Success := True;
      Result.Position := RB.Position;
      Result.Result := StringArray(RA.Result, RB.Result);
      RunPostProc(Result);
    end;
  end;
end;

{ TOrParser }

constructor TOrParser.Create(A, B: TParser);
begin
  FA := A;
  FB := B;
end;

function TOrParser.Run(Input: String; Position: Integer): TParseResult;
begin
  Result := FA.Run(Input, Position);
  if not Result.Success then
    Result := FB.Run(Input, Position);
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

end.

