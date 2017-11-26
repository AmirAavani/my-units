unit BinaryArithmeticCircuitUnit;
{$Assertions on}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseArithmeticCircuitUnit, BitVectorUnit, ClauseUnit,
  BigInt;

type

  {
  We assume the BitVectors represent the binary representation of numbers.
  This class assumes all numbers are non-negative integers.

    BitVector V, with size n, encodes the integer
      \sum V[i]* 2^i
    In other word, the first variable in V is True iff the integer represented
    by V is odd.
    The second variable in V is True iff the integer represented by V has one in
    its second bit.
    And so on.
  }
  { TBinaryArithmeticCircuit }

  TBinaryArithmeticCircuit = class(TBaseArithmeticCircuit)
  private
  public
    { Result = a + 1}
    function Incr(const a: TBitVector): TBitVector; override;
    { Result = a - 1}
    function Decr(const a: TBitVector): TBitVector; override;
    { Result = a + b}
    function Add(const a, b: TBitVector): TBitVector; override;
    { Result = a * b}
    function Mul(const a, b: TBitVector): TBitVector; override;

    function GenerateCarryForAdd(a, b, c: TLiteral): TLiteral;

    { Result is True iff a< b}
    function EncodeIsLessThan(const a, b: TBitVector): TLiteral; override;
    { Result is True iff a= b}
    procedure SubmitIsEqual(const a, b: TBitVector; l: TLiteral); override;

    function GenerateBinaryRep(const n: TBigInt; nbits: Integer = -1): TBitVector; override;
  end;

  function GetBinaryArithmeticCircuit: TBinaryArithmeticCircuit;

const
  VerbBinArithmCircuit: Integer = 2;

implementation

uses
  Math, TSeitinVariableUnit, SatSolverInterfaceUnit,
  ParameterManagerUnit;

function GetValueCount(SatSolver: TSATSolverInterface; v: TGroundBool;
  Args: array of const): Integer;
var
  i: Integer;
  lit : TLiteral;

begin
  Result := 0;
  for i := 0 to High(Args) do
  begin
    lit := TLiteral(Args[i].VInteger);
    if SatSolver.GetValue(lit) = v then
      Inc(Result);
  end;
end;

{ TBinaryArithmeticCircuit }

function TBinaryArithmeticCircuit.Incr(const a: TBitVector): TBitVector;
var
  One: TBitVector;
begin
  One := TBitVector.Create(a.Count, GetVariableManager.FalseLiteral);
  One[0] := GetVariableManager.TrueLiteral;

  Result := Add(a, One);

  One.Free;

end;

function TBinaryArithmeticCircuit.Decr(const a: TBitVector): TBitVector;
var
  Borrow: TBitVector;
  i, j: Integer;
  ai: TLiteral;

begin
  Borrow := TBitVector.Create(a.Count, GetVariableManager.FalseLiteral);

  Result := TBitVector.Create(a.Count, GetVariableManager.FalseLiteral);

  Borrow[0] := NegateLiteral(a[0]);
  Result[0] := NegateLiteral(a[0]);


  for i := 1 to a.Count - 1 do
  begin
    ai := a[i];

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(ai);
    SatSolver.AddLiteral(Borrow[i- 1]);
    Result[i] := SatSolver.GenerateXOrGate;

    { Borrow }
    // ~a[i] and Borrow[i- 1] <-> Borrow[i]

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(NegateLiteral(ai));
    SatSolver.AddLiteral(Borrow[i- 1]);
    Borrow[i] := SatSolver.GenerateAndGate;

  end;

  for i := 0 to a.Count - 1 do
    for j := i + 1 to a.Count - 1 do
    begin
      // ai & aj -> Result[j]
      // ~ai, ~aj, rj

      if Pos('Dec_1', GetRunTimeParameterManager.ValueByName['--ExtraClausesMode'])
              <> 0 then
      begin
        SatSolver.BeginConstraint;
        SatSolver.AddLiterals([NegateLiteral(a[i]),
                               NegateLiteral(a[j]),
                               Result[j]]);
        SatSolver.SubmitClause;
      end;
      //  ai & ~aj -> ~Result[j]
      // ~ai, aj, ~rj
      if Pos('Dec_2', GetRunTimeParameterManager.ValueByName['--ExtraClausesMode'])
              <> 0 then
      begin
        SatSolver.BeginConstraint;
        SatSolver.AddLiterals([NegateLiteral(a[i]),
                               a[j],
                               NegateLiteral(Result[j])]);
        SatSolver.SubmitClause;
      end;

    end;


  if (GetRunTimeParameterManager.Verbosity and VerbBinArithmCircuit) <> 0 then
    WriteLn('[Decr] a = ', a.ToString, ' Result= ', Result.ToString,
     'Borrow = ', Borrow.ToString);

  Borrow.Free;

end;

function TBinaryArithmeticCircuit.Add(const a, b: TBitVector): TBitVector;
var
  Carry: TBitVector;
  MaxLen: Integer;
  i: Integer;
  ai, bi: TLiteral;

begin
  assert(1 <= a.Count, 'a.Count (' + IntToStr(a.Count) + ') must be greater than 1.');
  assert(1 <= b.Count, 'b.Count (' + IntToStr(b.Count) + ') must be greater than 1.');

  MaxLen := Max(a.Count, b.Count);

  Carry := TBitVector.Create(MaxLen, GetVariableManager.FalseLiteral);
  Result := TBitVector.Create(MaxLen, GetVariableManager.FalseLiteral);

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(a[0]);
  SatSolver.AddLiteral(b[0]);
  Carry[0] := SatSolver.GenerateAndGate;

  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(a[0]);
  SatSolver.AddLiteral(b[0]);
  Result[0] := SatSolver.GenerateXOrGate;

  for i := 1 to MaxLen - 1 do
  begin
    ai := a.GetBitOrDefault(i, GetVariableManager.FalseLiteral);
    bi := b.GetBitOrDefault(i, GetVariableManager.FalseLiteral);

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(ai);
    SatSolver.AddLiteral(bi);
    SatSolver.AddLiteral(Carry[i- 1]);
    Result[i] := SatSolver.GenerateXOrGate;

    Carry[i] := GenerateCarryForAdd(ai, bi, Carry[i - 1]);
  end;

  if Carry[MaxLen- 1] <> TSeitinVariableUnit.GetVariableManager.FalseLiteral
     then
    Result.Add(Carry[MaxLen- 1]);

  if (StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
      VerbBinArithmCircuit)<> 0 then
  begin
    WriteLn('[Add]: a = ', a.ToString);
    WriteLn('[Add]: b = ', b.ToString);
    WriteLn('[Add]: Carry = ', Carry.ToString);
    WriteLn('[Add]: Result = ', Result.ToString);
  end;
  SatSolver.AddComment(a.ToString + ' + ' + b.ToString + ' = ' + Result.ToString);

  Carry.Free;
end;

function TBinaryArithmeticCircuit.Mul(const a, b: TBitVector): TBitVector;
  function ParallelAdder(Mat: TBitVectorList; Start, Finish: Integer): TBitVector;
  var
    Mid: Integer;
    FirstHalf, SecondHalf: TBitVector;

  begin
    if Start = Finish then
    begin
      Result := Mat[Start].Copy;

      if StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
       (1 shl VerbBinArithmCircuit)<> 0 then
        WriteLn('[Mul.ParallelAdder] Mat[', Start, '] = ', Result.ToString);
      Exit;
    end;


    Mid := (Start + Finish) div 2;
    FirstHalf := ParallelAdder(Mat, Start, Mid);
    if Mid < Finish then
      SecondHalf := ParallelAdder(Mat, Mid + 1, Finish)
    else
      SecondHalf := TBitVector.Create(Mat[0].Count, GetVariableManager.FalseLiteral);

    if StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
     (1 shl VerbBinArithmCircuit)<> 0 then
    begin
      WriteLn('[Mul.ParallelAdder] FirstHalf= ', FirstHalf.ToString);
      WriteLn('[Mul.ParallelAdder] SecondHalf= ', SecondHalf.ToString);
    end;

    Result := Self.Add(FirstHalf, SecondHalf);

    if StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
     (1 shl VerbBinArithmCircuit)<> 0 then
    begin
      WriteLn('[Mul.ParallelAdder] Mat[', Start, '] + ...+  Mat[', Finish, '] = ', Result.ToString);
    end;

    FirstHalf.Free;
    SecondHalf.Free;
  end;

var
  Mat: TBitVectorList;
  i, j: Integer;

begin
  Assert((1<= a.Count) and(1<= b.Count));

  Mat := TBitVectorList.Create;

  for i := 0 to a.Count - 1 do
    Mat.PushBack(TBitVector.Create(a.Count + b.Count, GetVariableManager.FalseLiteral));

  for i := 0 to a.Count- 1 do
  begin
   // Mat[i][k], k< i, False

   // Mat[i][i + j] <=> a[i] and b[j]
    for j := 0 to b.Count- 1 do
    begin
      //Mat[i][i+ j] := CreateLiteral(GetVariableManager.CreateNewVariable, False);
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral(a[i]);
      SatSolver.AddLiteral(b[j]);
      Mat[i][i+j] := SatSolver.GenerateAndGate;
    end;
  end;

  if StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
   (1 shl VerbBinArithmCircuit)<> 0 then
  begin
    for i := 0 to Mat.Count- 1 do
      WriteLn('[MUL]: Mat[', i, ']= ', Mat[i].ToString);
    WriteLn('[MUL]');
  end;

  Result := ParallelAdder(Mat, 0, Mat.Count- 1);

  Mat.Free;
  SatSolver.AddComment('TBinaryArithmeticCircuit: ' + a.ToString + ' * ' + b.ToString + ' = ' + Result.ToString);
end;

function TBinaryArithmeticCircuit.GenerateCarryForAdd(a, b, c: TLiteral): TLiteral;
var
  TrueCount, FalseCount: Integer;
begin
  SatSolver.BeginConstraint;
  SatSolver.AddLiteral(a);
  SatSolver.AddLiteral(b);
  SatSolver.AddLiteral(c);

  TrueCount := SatSolver.NoOfLiteralInTopConstraint[gbTrue];
  FalseCount := SatSolver.NoOfLiteralInTopConstraint[gbFalse];

  SatSolver.AbortConstraint;

  if 2 <= TrueCount then
    Exit(GetVariableManager.TrueLiteral);
  if 2 <= FalseCount then
    Exit(GetVariableManager.FalseLiteral);


  Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);

  { Carray }

  // a and b -> r    (~a, ~b, r)
  // a and c -> r    (~a, ~c, r)
  // b and c -> r    (~b, ~c, r)
  // r and ~a -> b   (~r, a, b)
  // r and ~a -> c   (~r, a, c)
  // r and ~c -> b   (~r. b, c)

  SatSolver.BeginConstraint;
  SatSolver.AddLiterals([NegateLiteral(a), NegateLiteral(b), Result]);
  SatSolver.SubmitClause;

  SatSolver.BeginConstraint;
  SatSolver.AddLiterals([NegateLiteral(a), NegateLiteral(c), Result]);
  SatSolver.SubmitClause;

  SatSolver.BeginConstraint;
  SatSolver.AddLiterals([NegateLiteral(b), NegateLiteral(c), Result]);
  SatSolver.SubmitClause;

  SatSolver.BeginConstraint;
  SatSolver.AddLiterals([NegateLiteral(Result), a, b]);
  SatSolver.SubmitClause;

  SatSolver.BeginConstraint;
  SatSolver.AddLiterals([NegateLiteral(Result), b, c]);
  SatSolver.SubmitClause;

  SatSolver.BeginConstraint;
  SatSolver.AddLiterals([NegateLiteral(Result), a, c]);
  SatSolver.SubmitClause;

end;

function TBinaryArithmeticCircuit.EncodeIsLessThan(const a, b: TBitVector): TLiteral;
var
  aCopy, bCopy: TBitVector;
  i: Integer;
  aIsLessThanb,
  aiIsEqbi, aiIsLbi,
  aIsEqbTillNow: TBitVector;

begin//a < b
  SatSolver.AddComment('[IsLessThan] a: ' + a.ToString);
  SatSolver.AddComment('[IsLessThan] b: ' + b.ToString);

  if(GetRunTimeParameterManager.Verbosity and(1 shl VerbBinArithmCircuit))<> 0 then
  begin
    WriteLn('[IsLessThan] a: ', a.ToString);
    WriteLn('[IsLessThan] b: ', b.ToString);
  end;

  if a.Count <> b.Count then
  begin
    aCopy := TBitVector.Create(Max(a.Count, b.Count),
      GetVariableManager.FalseLiteral);
    bCopy := TBitVector.Create(Max(a.Count, b.Count),
      GetVariableManager.FalseLiteral);

    for i := 0 to a.Count - 1 do
      aCopy[i] := a[i];
    for i := 0 to b.Count - 1 do
      bCopy[i] := b[i];
    Result := Self.EncodeIsLessThan(aCopy, bCopy);
    aCopy.Free;
    bCopy.Free;
    Exit;
  end;

  WriteLn('[TBinaryArithmeticCircuit.EncodeIsLessThan] This function can be improved!');
  Assert(a.Count = b.Count);

  aiIsEqbi := TBitVector.Create(a.Count, GetVariableManager.FalseLiteral);
  aiIsLbi := TBitVector.Create(a.Count, GetVariableManager.FalseLiteral);
  aIsLessThanB := TBitVector.Create(a.Count, GetVariableManager.FalseLiteral);
  aIsEqbTillNow := TBitVector.Create(a.Count, GetVariableManager.FalseLiteral);

  aIsEqbTillNow.Add(GetVariableManager.TrueLiteral);

  for i := a.Count - 1 downto 0 do
  begin
    GetSatSolver.BeginConstraint;
    GetSatSolver.AddLiteral(a[i]);
    GetSatSolver.AddLiteral(b[i]);
    aiIsEqbi[i] := NegateLiteral(GetSatSolver.GenerateXOrGate);

    GetSatSolver.BeginConstraint;
    GetSatSolver.AddLiteral(NegateLiteral(a[i]));
    GetSatSolver.AddLiteral(b[i]);
    aiIsLbi[i] := GetSatSolver.GenerateAndGate;

    GetSatSolver.BeginConstraint;
    GetSatSolver.AddLiteral(aiIsEqbi[i]);
    GetSatSolver.AddLiteral(aIsEqbTillNow[i + 1]);
    aIsEqbTillNow[i] := GetSatSolver.GenerateAndGate;

    GetSatSolver.BeginConstraint;
    GetSatSolver.AddLiteral(aiIsLbi[i]);
    GetSatSolver.AddLiteral(aIsEqbTillNow[i+ 1]);
    aIsLessThanb[i] := GetSatSolver.GenerateAndGate;
  end;

  if(GetRunTimeParameterManager.Verbosity and(1 shl VerbBinArithmCircuit))<> 0 then
  begin
    WriteLn('aiIsEqbi = ', aiIsEqbi.ToString);
    WriteLn('aiIsLbi = ', aiIsLbi.ToString);
    WriteLn('aIsLessThanb = ', aIsLessThanb.ToString);
    WriteLn('aIsEqbTillNow = ', aIsEqbTillNow.ToString);
    SatSolver.AddComment('aiIsEqbi = ' + aiIsEqbi.ToString);
    SatSolver.AddComment('aiIsLbi = ' + aiIsLbi.ToString);
    SatSolver.AddComment('aIsLessThanb = ' + aIsLessThanb.ToString);
    SatSolver.AddComment('aIsEqbTillNow = ' + aIsEqbTillNow.ToString);
  end;

  GetSatSolver.BeginConstraint;
  for i := 0 to a.Count - 1 do
    GetSatSolver.AddLiteral(aIsLessThanb[i]);
  Result := GetSatSolver.GenerateOrGate;

  if(GetRunTimeParameterManager.Verbosity and(1 shl VerbBinArithmCircuit))<> 0 then
  begin
    WriteLn('[IsLessThan] Result = ', LiteralToString(Result));
  end;

  aiIsEqbi.Free;
  aiIsLbi.Free;
  aIsEqbTillNow.Free;
  aIsLessThanb.Free;

end;

procedure TBinaryArithmeticCircuit.SubmitIsEqual(const a, b: TBitVector;
  l: TLiteral);
var
  i: Integer;
  SameithBit: TLiteral;

begin
  GetSatSolver.BeginConstraint;

  {
  WriteLn('[EncodeIsEqual] a = ', a.ToString);
  WriteLn('[EncodeIsEqual] b = ', b.ToString);
  }

  for i := Min(a.Count, b.Count) to
                    Max(a.Count, b.Count) - 1 do
  begin
    if i < a.Count then
      GetSatSolver.AddLiteral(NegateLiteral(a[i]))
    else
      GetSatSolver.AddLiteral(NegateLiteral(b[i]));
  end;

  for i := 0 to Min(a.Count, b.Count) - 1 do
  begin
    GetSatSolver.BeginConstraint;

    GetSatSolver.AddLiteral(a[i]);
    GetSatSolver.AddLiteral(b[i]);

    SameithBit := GetVariableManager.TrueLiteral;
    if SatSolver.GetLiteralValue(l) <> gbTrue then
      SameithBit := CreateLiteral(GetVariableManager.CreateNewVariable, False);

    GetSatSolver.SubmitEquivGate(SameithBit);//aiAndbi <=> ai <-> bi;

    GetSatSolver.AddLiteral(SameithBit);
  end;

  GetSatSolver.SubmitAndGate(l);//Result<=> \bigwedge_i SameithBit;
  if Pos(UpperCase('EQ:'), UpperCase(ParameterManagerUnit.GetRunTimeParameterManager.
    ValueByName['--ExtraClausesMode'])) <> 0 then
  begin
    for i := 0 to Min(a.Count- 1, b.Count- 1) do
    begin
// ~ai, bi, ~r : ai &~bi => ~r
      GetSatSolver.BeginConstraint;
      GetSatSolver.AddLiteral(NegateLiteral(a[i]));
      GetSatSolver.AddLiteral(b[i]);
      GetSatSolver.AddLiteral(NegateLiteral(l));
      GetSatSolver.SubmitClause;
// ai, ~bi, ~r  : ~ai & bi => ~r
      GetSatSolver.BeginConstraint;
      GetSatSolver.AddLiteral(a[i]);
      GetSatSolver.AddLiteral(NegateLiteral(b[i]));
      GetSatSolver.AddLiteral(NegateLiteral(l));
      GetSatSolver.SubmitClause;
    end;
  end;

end;

function TBinaryArithmeticCircuit.GenerateBinaryRep(const n: TBigInt;
  nbits: Integer): TBitVector;
var
  P2: TBigInt;
  BitCount: Integer;
  i: Integer;

begin
  P2 := BigIntFactory.GetNewMember.SetValue(2);
  BitCount := 1;

  while P2.CompareWith(n)<= 0 do
  begin
    Inc(BitCount);
    P2.Add(P2);
  end;
  BigIntFactory.ReleaseMember(P2);
  if BitCount < nbits then
    BitCount := nbits;

  if (GetRunTimeParameterManager.Verbosity and
    (1 shl VerbBinArithmCircuit)) <> 0 then
    WriteLn(BitCount, ' ', n.ToString, ' ', P2.ToString);

  Result := TBitVector.Create(BitCount, GetVariableManager.FalseLiteral);
  for i := 0 to BitCount- 1 do
    if n.CheckBit(i) then
      Result[i] := GetVariableManager.TrueLiteral
    else
      Result[i] := GetVariableManager.FalseLiteral;

end;

var
  BinaryArithmeticCircuit: TBinaryArithmeticCircuit;

function GetBinaryArithmeticCircuit: TBinaryArithmeticCircuit;
begin
  Result := BinaryArithmeticCircuit;

end;

initialization
  BinaryArithmeticCircuit := TBinaryArithmeticCircuit.Create;

finalization
  BinaryArithmeticCircuit.Free;

end.

