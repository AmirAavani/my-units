unit BaseLogicCircuits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseCircuitUnit, BitVectorUnit, ClauseUnit;

type

  { TBaseLogicCircuit }

  TBaseLogicCircuit = class(TBaseCircuit)
  public
    { Generate an encoding for
        "if condition then c = a else c = b;"
    }
    procedure ITE(const Condition: TLiteral; const a, b, c: TBitVector); virtual;
    function EncodeITE(const Condition: TLiteral; const a, b, c: TBitVector):
      TLiteral; virtual;
    function GenerateITE(const Condition: TLiteral; const a, b: TBitVector):
      TBitVector; virtual;
    { Generate an encoding for
        "if condition then c = a;"
    }
    procedure SetIf(const Condition: TLiteral; const a, c: TBitVector); virtual;
    function EncodeSetIf(const Condition: TLiteral; const a, c: TBitVector):
      TLiteral; virtual;
  end;

implementation
uses
    Math, TSeitinVariableUnit, SatSolverInterfaceUnit,
  ParameterManagerUnit;

{ TBaseLogicCircuit }

procedure TBaseLogicCircuit.ITE(const Condition: TLiteral; const a, b,
  c: TBitVector);
var
  MaxSize: Integer;
  i: Integer;
  la, lb, lc: TLiteral;
  p: TLiteral;

begin
  MaxSize := Max(a.Count, Max(b.Count, c.Count));

  for i := 0 to MaxSize do
  begin
    la := a.GetBitOrDefault(i, GetVariableManager.FalseLiteral);
    lb := b.GetBitOrDefault(i, GetVariableManager.FalseLiteral);
    lc := c.GetBitOrDefault(i, GetVariableManager.FalseLiteral);

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(Condition);
    SatSolver.AddLiteral(la);
    SatSolver.AddLiteral(lb);
    p := SatSolver.GenerateITEGate;

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(p);
    SatSolver.AddLiteral(lc);
    SatSolver.SubmitEquivGate(GetVariableManager.TrueLiteral);
  end;
end;

function TBaseLogicCircuit.EncodeITE(const Condition: TLiteral; const a, b,
  c: TBitVector): TLiteral;
var
  cPrime: TBitVector;
  i: Integer;
  Lit: TLiteral;

begin
  cPrime := TBitVector.Create(c.Count);

  Self.ITE(Condition, a, b, cPrime);

  SatSolver.BeginConstraint;
  for i := 0 to c.Count - 1 do
  begin
    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(c[i]);
    SatSolver.AddLiteral(cPrime[i]);
    Lit := SatSolver.GenerateXOrGate;

    SatSolver.AddLiteral(NegateLiteral(Lit));
  end;

  Result := SatSolver.GenerateAndGate;

end;

function TBaseLogicCircuit.GenerateITE(const Condition: TLiteral; const a,
  b: TBitVector): TBitVector;
begin
  Result := TBitVector.Create(max(a.Count, b.Count));
  Self.EncodeITE(Condition, a, b, Result);

end;

procedure TBaseLogicCircuit.SetIf(const Condition: TLiteral; const a,
  c: TBitVector);
var
  MaxSize: Integer;
  i: Integer;
  la, lc: TLiteral;

begin
  MaxSize := Max(a.Count, c.Count);

  for i := 0 to MaxSize do
  begin
  { condition & la => lc
    condition & ~la => ~lc
  }
    la := a.GetBitOrDefault(i, GetVariableManager.FalseLiteral);
    lc := c.GetBitOrDefault(i, GetVariableManager.FalseLiteral);

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(NegateLiteral(Condition));
    SatSolver.AddLiteral(NegateLiteral(la));
    SatSolver.AddLiteral(lc);
    SatSolver.SubmitClause;

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(NegateLiteral(Condition));
    SatSolver.AddLiteral(la);
    SatSolver.AddLiteral(NegateLiteral(lc));
    SatSolver.SubmitClause;
  end;

end;

function TBaseLogicCircuit.EncodeSetIf(const Condition: TLiteral; const a, c:
  TBitVector): TLiteral;
var
  cPrime: TBitVector;
  i: Integer;
  Lit: TLiteral;

begin
  cPrime := TBitVector.Create(c.Count);

  Self.SetIf(Condition, a, cPrime);

  SatSolver.BeginConstraint;
  for i := 0 to c.Count - 1 do
  begin
    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(c[i]);
    SatSolver.AddLiteral(cPrime[i]);
    Lit := SatSolver.GenerateXOrGate;

    SatSolver.AddLiteral(NegateLiteral(Lit));
  end;

  Result := SatSolver.GenerateAndGate;
end;

end.

