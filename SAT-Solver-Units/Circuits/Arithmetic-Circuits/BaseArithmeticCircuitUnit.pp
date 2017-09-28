unit BaseArithmeticCircuitUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseCircuitUnit, BitVectorUnit, ClauseUnit,
  SatSolverInterfaceUnit, fgl, BigInt, GenericCollectionUnit, Math;

type
  TBitVectorList= specialize TGenericCollection<TBitVector>;

  { TBaseArithmeticCircuit }

  {
  This class is the base class for arithmetic circuits. It does not
  have any assumption about the representation of numbers.
  }
  TBaseArithmeticCircuit = class(TBaseCircuit)
  private
  public
    {
    Generate an encoding for consraint \bar{a}[\tau] = n.
    }
    function GenerateBinaryRep(const n: TBigInt; nbits: Integer = -1): TBitVector; virtual; abstract;
    {
    Generate an encoding for constraint a+1=c.
    }
    function EncodeIncr(const a: TBitVector): TBitVector; virtual;
    {
    Generate an encoding for constraint a+b=c.
    }
    function EncodeAdd(const a, b: TBitVector): TBitVector; virtual;
    {
    Generate an encoding for constraint (a+b)/2=c:
      if a + b is even, c = (a + b) / 2
      if a + b is odd, c = (a + b - 1) / 2
    }
    function EncodeAvg(const a, b: TBitVector): TBitVector; virtual;
    {
    Generate an encoding for constraint a * b = c.
    }
    function EncodeMul(const a, b: TBitVector): TBitVector; virtual;

    {
    Generates appropriate set clauses(and submits each of them to
       SatSolver) such that the returning literal is true iff a< b.
    }
    function EncodeIsLessThan(const a, b: TBitVector): TLiteral; virtual; abstract;
    {
    Generates appropriate set clauses(and submits each of them to
       SatSolver) such that the returning literal is true iff a= b.
    }
    function EncodeIsEqual(const a, b: TBitVector): TLiteral; virtual;
    {
      Generates appropriate set clauses(and submits each of them to
      SatSolver) such that the returning literal is true iff a= b.

      }
    procedure SubmitIsEqual(const a, b: TBitVector; l: TLiteral); virtual; abstract;
    {
    Generates appropriate set clauses(and submits each of them to
       SatSolver) such that the returning literal is true iff a< b.
      The implememtation provided by the base class uses the following observation:
      a<= b iff a< b or a= b.
    }
    function EncodeIsLessThanOrEq(const a, b: TBitVector): TLiteral; virtual;
    {
    Generates appropriate set clauses(and submits each of them to
       SatSolver) such that the returning literal is true iff a> b.
      The implememtation provided by the base class uses the following observation:
      a> b iff not(a<= b).
    }
    function EncodeIsGreaterThan(const a, b: TBitVector): TLiteral; virtual;
    {
    Generates appropriate set clauses(and submits each of them to
       SatSolver) such that the returning literal is true iff a< b.
      The implememtation provided by the base class uses the following observation:
      a>= b iff not(a< b).
    }
    function EncodeIsGreaterThanOrEq(const a, b: TBitVector): TLiteral; virtual;

    {
    Generates appropriate set clauses(and submits each of them to
      SatSolver) such that
     we have the returned BitVector is the result of a+ 1
    }
    function Incr(const a: TBitVector): TBitVector; virtual; abstract;
    {
    Generates appropriate set clauses(and submits each of them to
      SatSolver) such that
     we have the returned BitVector is the result of a - 1
    }
    function Decr(const a: TBitVector): TBitVector; virtual; abstract;
    {
    Generates appropriate set clauses(and submits each of them to
      SatSolver) such that
     we have the returned BitVector is the result of a+ b
    }
    function Add(const a, b: TBitVector): TBitVector; virtual; abstract;
    {
    Generates appropriate set clauses(and submits each of them to
      SatSolver) such that
     we have the returned BitVector is the result of a+ b
    }
    function Sub(const a, b: TBitVector): TBitVector; virtual;
    {
    Generates appropriate set clauses(and submits each of them to
       SatSolver) such that
     we have the returned BitVector is the result of a+ b
    }
    function Mul(const a, b: TBitVector): TBitVector; virtual; abstract;
    {
    Generates appropriate set clauses(and submits each of them to
       the SatSolver) such that
     the returned BitVector is the result of a div b.
    }
    {
    Generates appropriate set clauses(and submits each of them to
      SatSolver) such that
     we have the returned BitVector is the result of (a + b) div 2.
    }
    function Avg(const a, b: TBitVector): TBitVector; virtual;

    function Divide(const a, b: TBitVector): TBitVector; virtual;
    {
    Generates appropriate set clauses(and submits each of them to
       the SatSolver) such that
     the returned BitVector is the result of a mod b.
    }

    function Remainder(const a, b: TBitVector): TBitVector; virtual;


    {
      Using a divide and conquer approach, this function builds a tower of
      adders whose depth is log of # of vectors in Nums.
    }
    function Add(Nums: TBitVectorList): TBitVector; virtual;

  end;

implementation

uses
  TSeitinVariableUnit;
{ TBaseArithmeticCircuit }

function TBaseArithmeticCircuit.EncodeIncr(const a: TBitVector): TBitVector;
begin
  Result := Self.Incr(a);
end;

function TBaseArithmeticCircuit.EncodeAdd(const a, b: TBitVector): TBitVector;
begin
  Result := Add(a, b);
end;

function TBaseArithmeticCircuit.EncodeAvg(const a, b: TBitVector): TBitVector;
begin
  Result := Self.Add(a, b);
  Result.Erase(0);
end;

function TBaseArithmeticCircuit.EncodeMul(const a, b: TBitVector): TBitVector;
begin
  Result := Mul(a, b);
end;

function TBaseArithmeticCircuit.EncodeIsEqual(const a, b: TBitVector): TLiteral;
begin
  Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);
  SubmitIsEqual(a, b, Result);
end;

function TBaseArithmeticCircuit.Remainder(const a, b: TBitVector): TBitVector;
var
  c, d, bd: TBitVector;
  EqLit, LeLit: TLiteral;

begin
  Result:= TBitVector.Create(b.Size);
//  WriteLn('[Remainder] Result = ', Result.ToString);

  d:= TBitVector.Create(a.Size);
//  WriteLn('[Remainder] d = ', d.ToString);
  bd:= Self.Mul(b, d);
//  WriteLn('[Remainder] b*d = ', bd.ToString);
  c:= Self.Add(bd, Result);
//  WriteLn('[Remainder] b*d + r= ', c.ToString);
  EqLit:= Self.EncodeIsEqual(a, c);

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(EqLit);
  SatSolver.SubmitClause;

  LeLit:= Self.EncodeIsLessThan(Result, b);
  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(LeLit);
  SatSolver.SubmitClause;


end;

function TBaseArithmeticCircuit.EncodeIsLessThanOrEq(const a, b: TBitVector
  ): TLiteral;
var
  l1, l2: TLiteral;

begin
  l1 := EncodeIsLessThan(a, b);
  l2 := EncodeIsEqual(a, b);

  Result := CreateLiteral(TSeitinVariableUnit.GetVariableManager.
                        CreateNewVariable, False);

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(l1);
  SatSolver.AddLiteral(l2);
  SatSolver.SubmitOrGate(Result);

end;

function TBaseArithmeticCircuit.EncodeIsGreaterThan(const a, b: TBitVector): TLiteral;
begin
  Result := NegateLiteral(Self.EncodeIsLessThanOrEq(a, b));

end;

function TBaseArithmeticCircuit.EncodeIsGreaterThanOrEq(const a, b: TBitVector
  ): TLiteral;
begin
  Result := NegateLiteral(EncodeIsLessThan(a, b));

end;

{
  Instead of encoding c=a- b, the sub function encodes
 (a'= b+ c) and(a= a')
}
function TBaseArithmeticCircuit.Sub(const a, b: TBitVector): TBitVector;
var
  aPrime: TBitVector;
  aPrimeEqa: TLiteral;

begin
  Result := TBitVector.Create(a.Size);

  aPrime := Self.Add(b, Result);

  aPrimeEqa := Self.EncodeIsEqual(aPrime, a);

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(aPrimeEqa);
  SatSolver.SubmitClause;


end;

function TBaseArithmeticCircuit.Avg(const a, b: TBitVector): TBitVector;
begin
  Result := Self.Add(a, b);
  Result.Erase(0);
  if Result.Count = 0 then
    Result.Add(GetVariableManager.FalseLiteral);
end;

function TBaseArithmeticCircuit.Divide(const a, b: TBitVector): TBitVector;
var
  aPrime: TBitVector;
  aPrimeEqa: TLiteral;

begin
  Result := TBitVector.Create(a.Size);

  aPrime := Self.Mul(b, Result);

  aPrimeEqa := Self.EncodeIsEqual(aPrime, a);

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(aPrimeEqa);
  SatSolver.SubmitClause;

end;

function TBaseArithmeticCircuit.Add(Nums: TBitVectorList): TBitVector;

  function RecBuild(Low, High: Integer): TBitVector;
  var
    Left, Right: TBitVector;

  begin
    if Low = High then
      Result  := Nums [Low]
    else
    begin
      Left := RecBuild(Low,(Low + High) div 2);
      Right := RecBuild((Low + High) div 2 + 1, High);

      Result := Self.Add(Left, Right);

    end;

  end;

begin
  if Nums.Count = 2 then
    Result := Self.Add(Nums[0], Nums[1])
  else
    Result := RecBuild(0, Nums.Count - 1);

end;


end.

