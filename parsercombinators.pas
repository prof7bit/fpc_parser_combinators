{ experimantal parser combinator framework
  with runnables instead of lambdas to see
  how far object pascal can be pushed

  dedicated to the public domain,
  prof7bit@gmail.com
}
unit parsercombinators;

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

  IParser = interface
    procedure SetPostProc(P: TPostProc);
    procedure SetPostMeth(P: TPostMeth);
    procedure SetImplementation(Impl: IParser);
    function Run(Input: String; Position: Integer): TParseResult;
  end;

  { TParser }

  TParser = class(TInterfacedObject, IParser)
    FPostProc: TPostProc;
    FPostMeth: TPostMeth;
    FImpl: IParser;
    procedure SetPostProc(P: TPostProc);
    procedure SetPostMeth(P: TPostMeth);
    procedure SetImplementation(Impl: IParser);
    function Run(Input: String; Position: Integer): TParseResult; virtual;
    procedure RunPostProc(var Result: TParseResult);
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
    FA, FB: IParser;
    constructor Create(A, B: IParser);
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;

  { TAndParser }

  TAndParser = class(TParser)
    FA, FB: IParser;
    constructor Create(A, B: IParser);
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;


  operator or(A, B: IParser): IParser;
  operator and(A, B: IParser): IParser;

  function Lit(S: String): IParser;
  function White: IParser;
  function Sym(S: String): IParser;
  function Num: IParser;

implementation

operator or(A, B: IParser): IParser;
begin
  Result := TOrParser.Create(A, B);
end;

operator and(A, B: IParser): IParser;
begin
  Result := TAndParser.Create(A, B);
end;

function Lit(S: String): IParser;
begin
  Result := TLitParser.Create(S);
end;

function White: IParser;
begin
  Result := TWhitespaceParser.Create;
end;

function Sym(S: String): IParser;
begin
  Result := White and Lit(S);
end;

function Num: IParser;
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

procedure TParser.SetPostProc(P: TPostProc);
begin
  FPostProc := P;
end;

procedure TParser.SetPostMeth(P: TPostMeth);
begin
  FPostMeth := P;
end;

procedure TParser.SetImplementation(Impl: IParser);
begin
  FImpl := Impl;
end;

function TParser.Run(Input: String; Position: Integer): TParseResult;
begin
  if not Assigned(FImpl) then
    raise Exception.Create('empty parser without implementation was called');
  Result := FImpl.Run(Input, Position);
  RunPostProc(Result);
end;

procedure TParser.RunPostProc(var Result: TParseResult);
begin
  if Assigned(FPostProc) then FPostProc(Result);
  if Assigned(FPostMeth) then FPostMeth(Result);
end;

{ TAndParser }

constructor TAndParser.Create(A, B: IParser);
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

constructor TOrParser.Create(A, B: IParser);
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

