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
    function EncodeITE(const Condition: TLiteral; const a, b: TBitVector):
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

function TBaseLogicCircuit.EncodeITE(const Condition: TLiteral; const a, b:
  TBitVector) : TBitVector;
var
  MaxSize: Integer;
  i: Integer;
  la, lb, lc: TLiteral;
  p: TLiteral;

begin
  MaxSize := Max(a.Count, b.Count);
  Result := TBitVector.Create(MaxSize, GetVariableManager.FalseLiteral);

  for i := 0 to MaxSize - 1 do
  begin
    la := a.GetBitOrDefault(i, GetVariableManager.FalseLiteral);
    lb := b.GetBitOrDefault(i, GetVariableManager.FalseLiteral);

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(Condition);
    SatSolver.AddLiteral(la);
    SatSolver.AddLiteral(lb);
    Result[i] := SatSolver.GenerateITEGate;
  end;
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

