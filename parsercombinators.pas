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
  TNodeType = (
    ntEmpty,
    ntString,
    ntInt,
    ntFloat,
    ntBranch
  );

  TNodeData = record
    Typ: TNodeType;
    Int: Integer;
    Float: Double;
    Str: String;
    Child: array of TNodeData;
  end;

  TParseResult = record
    Position: Integer;
    Success: Boolean;
    Data: TNodeData;
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


  procedure DebugPrint(Data: TNodeData; Ind: Integer = 0);

implementation

procedure DebugPrint(Data: TNodeData; Ind: Integer = 0);
var
  I: Integer;
  D: TNodeData;
begin
  for I := 0 to Ind do
    Write(' ');
  case Data.Typ of
    ntEmpty: begin
       WriteLn('ntEmpty');
    end;
    ntString: begin
       WriteLn('ntString: ', Data.Str);
    end;
    ntInt: begin
       WriteLn('ntInt: ', Data.Int);
    end;
    ntFloat: begin
       WriteLn('ntFloat: ', Data.Float);
    end;
    ntBranch: begin
       WriteLn('ntBranch:');
       for D in Data.Child do begin
         DebugPrint(D, Ind + 4);
       end;
    end;
  end;
end;

{ combine two nodes into one branch node, that is a node of type ntBranch
  with the contents of A and B in its leaves. If A or B is ntEmpty then it
  will NOT make a branch, instead it will just return the other one unchanged }
function MakeBranch(A, B: TNodeData): TNodeData;
begin
  if A.Typ = ntEmpty then
    Result := B
  else if B.Typ = ntEmpty then
    Result := A
  else begin
    SetLength(Result.Child, 2);
    Result.Child[0] := A;
    Result.Child[1] := B;
    Result.Typ := ntBranch;
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
    Result.Data.Str := Copy(Input, Position, Result.Position - Position);
    Result.Data.Int := StrToInt(Result.Data.Str);
    Result.Data.Typ := ntInt;
    RunPostProc(Result);
  end;
end;


{ TDigitParser }

function TDigitParser.Run(Input: String; Position: Integer): TParseResult;
begin
  if Input[Position] in ['0'..'9'] then begin
    Result.Success := True;
    Result.Position := Position + 1;
    Result.Data.Str := Input[Position];
    Result.Data.Int := StrToInt(Result.Data.Str);
    Result.Data.Typ := ntInt;
    RunPostProc(Result);
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
  Result.Data.Typ := ntEmpty;
  RunPostProc(Result);
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
      Result.Data := MakeBranch(RA.Data, RB.Data);
      Result.Position := RB.Position;
      Result.Success := True;
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
  if Result.Success then begin
    RunPostProc(Result);
  end;
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
      Result.Data.Typ := ntString;
      Result.Data.Str := FLit;
      RunPostProc(Result);
    end;
  end;
end;

end.

