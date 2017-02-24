unit parsers;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils;

type

  { TParseResult }

  TParseResult = Object
    Position: SizeInt;
    Result: TBytes;
    Success: Boolean;
    procedure Print;
  end;

  { IParser }

  IParser = interface
    function Run(Data: TBytes; Position: SizeInt): TParseResult;
  end;

  TParser = Class(TInterfacedObject, IParser)
    function Run(Data: TBytes; Position: SizeInt): TParseResult; virtual; abstract;
  end;

  { TLitParser }

  TLitParser = Class(TParser)
    FB: Byte;
    constructor Create(B: Byte);
    function Run(Data: TBytes; Position: SizeInt): TParseResult; override;
  end;


  { TWhitespaceParser
    Skips over any whitespace and always returns success}

  TWhitespaceParser = Class(TParser)
    function Run(Data: TBytes; Position: SizeInt): TParseResult; override;
  end;


  { TOrCombinator }

  TOrCombinator = class(TParser)
    FP1: IParser;
    FP2: IParser;
    constructor Create(P1, P2: IParser);
    function Run(Data: TBytes; Position: SizeInt): TParseResult; override;
  end;

  { TAndCombinator }

  TAndCombinator = class(TParser)
    FP1: IParser;
    FP2: IParser;
    constructor Create(P1, P2: IParser);
    function Run(Data: TBytes; Position: SizeInt): TParseResult; override;
  end;


  { internally everything operates on TBytes instead of String, so we provide
    a bunch of convenience functions and operators for the application to work
    with TBytes and convert back and forth between them and other types }
  function TBytes(B: array of Byte): TBytes;
  function TBytes(S: String): TBytes;
  function ToString(B: TBytes): String;
  operator + (A, B: TBytes): TBytes;
  operator + (A: TBytes; B: Byte): TBytes;

  { The functions and operators below don't parse anything but merely
    create new IParser objects from a combination of existing IParser
    objects and/or from other parameters, so a huge complex IParser can
    be created out of a combination of many small elementary IParsers }
  function Lit(C: Char): IParser;
  function Lit(S: String): IParser;
  function Whitespace: IParser;
  function Symbol(C: Char): IParser;
  function Symbol(S: String): IParser;
  operator or(P1, P2: IParser): IParser;
  operator and(P1, P2: IParser): IParser;


implementation


{ TWhitespaceParser }

function TWhitespaceParser.Run(Data: TBytes; Position: SizeInt): TParseResult;
begin
  while Position < Length(Data) do begin
    if Chr(Data[Position]) in [' ', #13, #10, #9] then
      Inc(Position)
    else
      break;
  end;
  Result.Position := Position;
  Result.Success := True;
end;

{ TAndCombinator }

constructor TAndCombinator.Create(P1, P2: IParser);
begin
  FP1 := P1;
  FP2 := P2
end;

function TAndCombinator.Run(Data: TBytes; Position: SizeInt): TParseResult;
var
  R1, R2: TParseResult;
begin
  Result.Position := Position;
  Result.Success := False;
  R1 := FP1.Run(Data, Position);
  if R1.Success then begin
    R2 := FP2.Run(Data, R1.Position);
    if R2.Success then begin
      Result.Success := True;
      Result.Position := R2.Position;
      Result.Result := R1.Result + R2.Result;
    end;
  end;
end;

{ TOrCombinator }

constructor TOrCombinator.Create(P1, P2: IParser);
begin
  FP1 := P1;
  FP2 := P2;
end;

function TOrCombinator.Run(Data: TBytes; Position: SizeInt): TParseResult;
begin
  Result := FP1.Run(Data, Position);
  if not Result.Success then begin
    Result := FP2.Run(Data, Position);
  end;
end;

{ TLitParser }

constructor TLitParser.Create(B: Byte);
begin
  FB := B;
end;

function TLitParser.Run(Data: TBytes; Position: SizeInt): TParseResult;
begin
  if Data[Position] = FB then begin
    Result.Result := TBytes([FB]);
    Result.Position := Position + 1;
    Result.Success := True;
  end
  else begin
    Result.Position := Position;
    Result.Success := False;
  end;
end;



{ TParseResult }

procedure TParseResult.Print;
var
  B: Byte;
begin
  Writeln(Success);
  Writeln(Position);
  WriteLn(Length(Result));
  for B in Result do begin
    Write(Chr(B), ' ');
  end;
  Writeln;
end;

{ helpers and operators }

function TBytes(B: array of Byte): TBytes;
begin
  SetLength(Result, Length(B));
  Move(B[0], Result[0], Length(B));
end;

function TBytes(S: String): TBytes;
begin
  SetLength(Result, Length(S));
  Move(S[1], Result[0], Length(S));
end;

function Lit(C: Char): IParser;
begin
  Result := TLitParser.Create(Ord(C));
end;

function Lit(S: String): IParser;
var
  I: SizeInt;
begin
  Result := Lit(S[1]);
  for I := 2 to Length(S) do begin
    Result := Result and Lit(S[I]);
  end;
end;

function ToString(B: TBytes): String;
begin
  SetLength(Result, Length(B));
  Move(B[0], Result[1], Length(B));
end;

operator + (A, B: TBytes): TBytes;
begin
  SetLength(Result, Length(A) + Length(B));
  Move(A[0], Result[0], Length(A));
  Move(B[0], Result[Length(A)], Length(B));
end;

operator + (A: TBytes; B: Byte): TBytes;
begin
  SetLength(Result, Length(A) + 1);
  Move(A[0], Result[0], Length(A));
  Result[Length(A)] := B;
end;

function Whitespace: IParser;
begin
  Result := TWhitespaceParser.Create;
end;

function Symbol(C: Char): IParser;
begin
  Result := Whitespace and Lit(C);
end;

function Symbol(S: String): IParser;
begin
  Result := Whitespace and Lit(S);
end;

operator or(P1, P2: IParser): IParser;
begin
  Result := TOrCombinator.Create(P1, p2);
end;

operator and(P1, P2: IParser): IParser;
begin
  Result := TAndCombinator.Create(P1, P2);
end;

end.

