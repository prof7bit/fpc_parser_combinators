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
  Classes, SysUtils, variants;

type
  TParseResult = record
    Success: Boolean;
    Position: Integer;
    Result: Variant;
  end;

  TPostProc = procedure(var Result: TParseResult);
  TPostMeth = procedure(var Result: TParseResult) of object;

  { TParser }

  TParser = class
    FPostProc: TPostProc;
    FPostMeth: TPostMeth;
    FImpl: TParser;
    FDestroying: Boolean;
    function IsDestroying: Boolean;
    procedure SetPostProc(P: TPostProc);
    procedure SetPostMeth(P: TPostMeth);
    procedure SetImplementation(Impl: TParser);
    function Run(Input: String; Position: Integer): TParseResult; virtual;
    procedure RunPostProc(var Result: TParseResult);
    destructor Destroy; override;
  end;

  { TTwoParsers }

  TTwoParsers = class(TParser)
    FA: TParser;
    FB: TParser;
    constructor Create(A, B: TParser);
    destructor Destroy; override;
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

  TOrParser = class(TTwoParsers)
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;

  { TAndParser }

  TAndParser = class(TTwoParsers)
    function Run(Input: String; Position: Integer): TParseResult; override;
  end;


  operator or(A, B: TParser): TParser;
  operator and(A, B: TParser): TParser;

  function Lit(S: String): TParser;
  function White: TParser;
  function Sym(S: String): TParser;
  function Num: TParser;

implementation

{ helper functions }

function VarArrayLength(V: Variant): Integer;
begin
  if VarIsArray(V) then
    Result := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1
  else
    raise Exception.Create('trying to get the length of a non-array');
end;

procedure VarArraySetLength(var V: Variant; Len: Integer);
begin
  if VarIsArray(V) then
    VarArrayRedim(V, Len - 1)
  else
    raise Exception.Create('trying to set the length of a non-array');
end;

function VarArrayConcat(A, B: Variant): Variant;
var
  LenA, LenB: Integer;
  I: Integer;
begin
  if VarIsEmpty(A) then
    Result := B
  else if VarIsEmpty(B) then
    Result := A
  else begin
    if VarIsArray(A) then begin
      Result := A;
      LenA := VarArrayLength(A);
    end
    else begin
      Result := VarArrayCreate([0, 0], varvariant);
      Result[0] := A;
      LenA := 1;
    end;

    if VarIsArray(B) then begin
      LenB := VarArrayLength(B);
      VarArraySetLength(Result, LenA + LenB);
      for I := 0 to LenB - 1 do
        Result[LenA + I] := B[I];
    end
    else begin
      VarArraySetLength(Result, LenA + 1);
      Result[LenA] := B;
    end;
  end;
end;

procedure TryFree(var P: TParser);
begin
  if Assigned(P) and not P.IsDestroying then begin
    P.Free;
    P := nil;
  end;
end;

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

{ TParser }

function TParser.IsDestroying: Boolean;
begin
  Result := FDestroying;
end;

procedure TParser.SetPostProc(P: TPostProc);
begin
  FPostProc := P;
end;

procedure TParser.SetPostMeth(P: TPostMeth);
begin
  FPostMeth := P;
end;

procedure TParser.SetImplementation(Impl: TParser);
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

destructor TParser.Destroy;
begin
  FDestroying := True;
  TryFree(FImpl);
end;

{ TTwoParsers }

constructor TTwoParsers.Create(A, B: TParser);
begin
  FA := A;
  FB := B;
end;

destructor TTwoParsers.Destroy;
begin
  FDestroying := True;
  TryFree(FA);
  TryFree(FB);
  inherited Destroy;
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
    Result.Result := StrToInt(Copy(Input, Position, Result.Position - Position));
    RunPostProc(Result);
  end;
end;


{ TDigitParser }

function TDigitParser.Run(Input: String; Position: Integer): TParseResult;
begin
  if Input[Position] in ['0'..'9'] then begin
    Result.Success := True;
    Result.Position := Position + 1;
    Result.Result := StrToInt(Input[Position]);
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
  Result.Result := Unassigned;
  RunPostProc(Result);
end;

{ TAndParser }

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
      //WriteLn('a: ', FA.ClassName, ' ',  RA.Result, ' b: ', FB.ClassName, ' ', RB.Result);
      Result.Result := VarArrayConcat(RA.Result, RB.Result);
      RunPostProc(Result);
    end;
  end;
end;

{ TOrParser }

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
      Result.Result := FLit;
      RunPostProc(Result);
    end;
  end;
end;

end.

