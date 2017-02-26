{ experimantal parser combinator demo
  with runnables instead of lambdas to see
  how far object pascal can be pushed

  dedicated to the public domain,
  prof7bit@gmail.com
}
program parsecomb;

{$mode objfpc}{$H+}{$assertions on}

uses
  //heaptrc,
  Classes, sysutils, parsercombinators;

procedure Print(Res: TParseResult; Ind: Integer = 0);
var
  I: Integer;
  R: TParseResult;
begin
  if Ind = 0 then begin
    Writeln('-------------');
    Writeln('Success: ', Res.Success);
    Writeln('Position: ', Res.Position);
  end;
  for I := 0 to Ind do
    Write(' ');
  case Res.Typ of
    ntEmpty: begin
       WriteLn('ntEmpty');
    end;
    ntString: begin
       WriteLn('ntString: ', Res.Str);
    end;
    ntInt: begin
       WriteLn('ntInt: ', Res.Int);
    end;
    ntFloat: begin
       WriteLn('ntFloat: ', Res.Float);
    end;
    ntBranch: begin
       WriteLn('ntBranch:');
       for R in Res.Branch do begin
         Print(R, Ind + 4);
       end;
    end;
  end;
end;

procedure ProcExpr(var R: TParseResult);
var
  R1: TParseResult;
begin
  if R.Typ = ntBranch then begin
    //Print(R);
    { The structure of this node is
    ntBranch:
        ntBranch:
            ntString: (
            ntInt: 5
        ntString: )

    replace this entire branch with only the node
    found between the two parenthesis }
    R1 := R.Branch[0].Branch[1];

    // fixme: isn't there a better way?
    // why can't I just use R := R1?
    R.Typ := R1.Typ;
    R.Str := R1.Str;
    R.Int := R1.Int;
    R.Float := R1.Float;
    R.Branch := R1.Branch;
  end;
end;

procedure ProcAdd(var R: TParseResult);
begin
  //Print(R);
  { the structure of this node is:
  ntBranch:
      ntBranch:
          ntString: add
          ntInt: 2
      ntInt: 3

  replace this entire ntBranch with a single
  ntInt leaf containing only the calculated result }
  R.Int := R.Branch[0].Branch[1].Int + R.Branch[1].Int;
  R.Typ := ntInt;
  SetLength(R.Branch, 0);
end;

procedure ProcMul(var R: TParseResult);
begin
  //Print(R);
  { the structure of this node is:
  ntBranch:
      ntBranch:
          ntString: mul
          ntInt: 2
      ntInt: 3

  replace this entire ntBranch with a single
  ntInt leaf containing only the calculated result }
  R.Int := R.Branch[0].Branch[1].Int * R.Branch[1].Int;
  R.Typ := ntInt;
  SetLength(R.Branch, 0);
end;

var
  A: String;
  R: TParseResult;
  EXPR, PARENS, INNER, MULFUNC, ADDFUNC, _PARENS: TParser;

begin
  // there is recursion in the grammar, so we need a
  // placeholder (kind of like a forward declaration)
  _PARENS := TParser.Create;

  // define the grammar
  EXPR      := Num or _PARENS;
  MULFUNC   := Sym('mul') and EXPR and EXPR;
  ADDFUNC   := Sym('add') and EXPR and EXPR;
  INNER     := MULFUNC or ADDFUNC or Expr;
  PARENS    := Sym('(') and INNER and Sym(')');

  // solve the forward declaration, closing the circle
  _PARENS.SetImplementation(PARENS);

  // some of the above parsers need to perform
  // some action, otherwise it would be boring.

  EXPR.SetPostProc(@ProcExpr);
  ADDFUNC.SetPostProc(@ProcAdd);
  MULFUNC.SetPostProc(@ProcMul);

  R := EXPR.Run('(mul (add (add 200 2) (mul 2 2)) 34)', 1);
  Print(R);
  Assert(R.Success);
  Assert(R.Typ = ntInt);
  Assert(R.Int = 7004);

  repeat
    WriteLn('Type something that evals to 42 to quit');
    ReadLn(A);
    R := EXPR.Run(A, 1);
    Print(R);
  until R.Success and (R.Typ = ntInt) and (R.Int = 42);

  // freeing the root of the parser tree will free all
  // contained parsers recursively, even in the presence
  // of cyclic references, heaptrc should indicate no
  // leaks anymore after the call below
  EXPR.Free;
end.

