unit ModuloBasedFactoringUnit;

{$mode objfpc}{$H+}
{$Assertions on}
interface

uses
  Classes, SysUtils, FactoringUsingSATUnit, BigInt,
  BitVectorUnit, BinaryArithmeticCircuitUnit, gvector,
  ClauseUnit, GenericCollectionUnit;

type
  TIntegerCollection = specialize TGenericCollection<Integer>;

  { TModuloBasedBinaryArithmeticCircuit }

  TModuloBasedBinaryArithmeticCircuit = class(TBinaryArithmeticCircuit)
  private
    function GenerateAllPrimesLessThanEq(Last: Integer): TIntegerCollection;

  protected
    function GenerateModulosUsingDP(n: Integer): TIntegerCollection;
    function GenerateModulos1(n: Integer; MaxModLogVal: Integer = -1): TIntegerCollection;
    function GenerateModulos2(n: Integer; MaxModLogVal: Integer = -1): TIntegerCollection;
    function GenerateModulos3(n: Integer): TIntegerCollection;
    function GenerateModulos4(n: Integer): TIntegerCollection;
    function GenerateModulos5(n: Integer): TIntegerCollection;
    // Constant number of primes
    function GenerateModulos6(n: Integer): TIntegerCollection;
    function EncodeMul(const a, b, c: TBitVector; Level: Integer): TLiteral; override;
    {
    Encode c = a mod (2^m-1).
    }
    function EncodeMod(const a: TBitVector; const m: Int64; const c : TBitVector): TLiteral;
    {
    Encode a > (2^m-1).
    }
    function EncodeGreaterThan(const a: TBitVector; const m: Int64): TLiteral;
    {
      Encode c = (a + b) mod  (2^m -1)
    }
    function EncodeAddModuloMod(const a, b, c: TBitVector; m: Integer): TLiteral;
    {
      Result = (a + b) mod  (2^m -1)
    }
    function AddModuloMod(const a, b: TBitVector; m: Integer): TBitVector;
    {
    Encode Result = a mod (2^m-1).
    }
    function Modulo(const a: TBitVector; const m: Int64): TBitVector;

    {
    Encode Result = a + 2^m - 1
    }
    function IncByTwoToM_1(const a: TBitVector; const m: Int64): TBitVector;

  public
    constructor Create;
    destructor Destroy; override;



  end;

  { TBinaryModuloFactorizer }

  TBinaryModuloFactorizer= class(TBaseFactorizerUsingSAT)
  public
    constructor Create;
    destructor Destroy; override;

  end;

 const
   VerbModuloBasedBinaryArithmeticCircuit: Integer = 32;
   VerbModulo_ModuloBasedBinaryArithmeticCircuit: Integer = 64;

implementation
uses
  TSeitinVariableUnit, ParameterManagerUnit, math,
  SatSolverInterfaceUnit;

{ TModuloBasedBinaryArithmeticCircuit }
function ComputeModuloProduct(Bases: TIntegerCollection): TBigInt;
var
  Modulos: TBigIntCollection;
  i: Integer;

begin
  Modulos := TBigIntCollection.Create;

  for i := 0 to Bases.Size - 1 do
    Modulos.Add(BigIntFactory.GetNewMember.SetValue(1).
        ShiftLeft(Bases[i]).Decr);

  Result := BigIntFactory.ComputeProduct(Modulos);
  for i := 0 to Bases.Size - 1 do
    BigIntFactory.ReleaseMember(Modulos[i]);
  Modulos.Free;
end;

function SelectSmallestSubset(Modulos: TIntegerCollection;
  TargetLog: Integer): TIntegerCollection;
var
  CurrentSet: TIntegerCollection;
  MinSetMask: Integer;
  MinProd, Temp: TBigInt;
  i, j: Integer;
  mCount: Integer;

begin
  mCount := Modulos.Size;

  MinProd := ComputeModuloProduct(Modulos);
  Assert(TargetLog <= MinProd.Log, 'TargetLog = ' + IntToStr(TargetLog) +
    ' all modulos prod = ' + IntToStr(MinProd.Log));
  MinSetMask := (1 shl mCount) - 1;

  CurrentSet := TIntegerCollection.Create;
  for i := 0 to ((1 shl mCount) - 1) do
  begin
    CurrentSet.Clear;

    for j := 0 to mCount - 1 do
      if (i and (1 shl j)) <> 0 then
        CurrentSet.PushBack(Modulos[j]);

    Temp := ComputeModuloProduct(CurrentSet);
    if Temp.Log < TargetLog then
    else if MinProd.CompareWith(Temp) > 0 then
    begin
      BigIntFactory.ReleaseMember(MinProd);
      MinProd := Temp.Copy;
      MinSetMask := i;
    end;

    BigIntFactory.ReleaseMember(Temp);
  end;
  CurrentSet.Free;

  Result := TIntegerCollection.Create;
  for j := 0 to mCount - 1 do
    if (MinSetMask and (1 shl j)) <> 0 then
      Result.PushBack(Modulos[j]);
end;


function TModuloBasedBinaryArithmeticCircuit.GenerateModulos1(n: Integer;
  MaxModLogVal: Integer): TIntegerCollection;

var
  AllPrimes, Modulos: TIntegerCollection;
  b: Integer;
  logn: Integer;
  i, j: Integer;
  ActivePrime: Integer;

  Top, Bot, Mid: Integer;
  StartingIndex: Integer;

  Prod, t2p, Temp: TBigInt;
begin
  if MaxModLogVal = -1 then
    MaxModLogVal := MaxInt;

  AllPrimes := GenerateAllPrimesLessThanEq(2 * n);

  logn := 2 * Round(log2(n / 2.0));
  if (GetRunTimeParameterManager.Verbosity and
                  VerbModuloBasedBinaryArithmeticCircuit) <> 0 then
    WriteLn('logn = ', logn);

  b := Round((3 * n + 1) / (2.0 * logn)) div 2;
//  p2b := 1 shl b;

  Top := AllPrimes.Size - 1; Bot := 0;

  StartingIndex := -1;
  while Bot <= Top do
  begin
    Mid := (Top + Bot) div 2;
    if AllPrimes[Mid] < b then
      Bot := Mid + 1
    else // if p2b <= AllPrimes[Mid]
    begin
      StartingIndex := Mid;
      Top := Mid - 1;
    end;
  end;

  Prod := BigIntFactory.GetNewMember.SetValue(1);

  Modulos := TIntegerCollection.Create;
  for i := StartingIndex downto Max(StartingIndex -  logn, 0)  do
  begin
    if MaxModLogVal <= AllPrimes[i]  then
      break;
    ActivePrime := AllPrimes[i];
    if ActivePrime = 2 then
      ActivePrime := 4
    else if ActivePrime = 3 then
      ActivePrime := 9;

    t2p := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(ActivePrime).Decr;
    assert(not t2p.IsZero);

    Temp := Prod.Mul(t2p);
    BigIntFactory.ReleaseMember(t2p);
    BigIntFactory.ReleaseMember(Prod);
    Prod := Temp;

    Modulos.PushBack(ActivePrime);
    if n < Prod.Log then
      Break;

  end;

  while Prod.Log <= n do
  begin
    i := StartingIndex;
    while True do
    begin
      Inc(i);
      if MaxModLogVal <= AllPrimes[i]  then
        break;
      ActivePrime := AllPrimes[i];
      if ActivePrime = 2 then
        ActivePrime := 4
      else if ActivePrime = 3 then
        ActivePrime := 9;

      t2p := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(ActivePrime).Decr;
      assert(not t2p.IsZero);

      Temp := Prod.Mul(t2p);
      BigIntFactory.ReleaseMember(t2p);
      BigIntFactory.ReleaseMember(Prod);
      Prod := Temp;

      Modulos.PushBack(ActivePrime);
      if n < Prod.Log then
        Break;

    end;

  end;
  if Prod.Log < n then
  begin
    for i := StartingIndex - 1 downto Max(0, Modulos.Size - Logn)  do
    begin
      ActivePrime := AllPrimes[i];
      if ActivePrime = 2 then
        ActivePrime := 4
      else if ActivePrime = 3 then
        ActivePrime := 9;

      t2p := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(ActivePrime).Decr;
      Temp := Prod.Mul(t2p);
      BigIntFactory.ReleaseMember(t2p);
      BigIntFactory.ReleaseMember(Prod);
      Prod := Temp;

      Modulos.PushBack(AllPrimes[i]);
      if n < Prod.Log then
        Break;
    end;
  end;

  BigIntFactory.ReleaseMember(Prod);
  AllPrimes.Free;

  Result := SelectSmallestSubset(Modulos, n);
  Modulos.Free;

end;

function TModuloBasedBinaryArithmeticCircuit.GenerateModulos2(n: Integer;
  MaxModLogVal: Integer): TIntegerCollection;

  function SelectSmallestSubset(Modulos: TIntegerCollection;
    TargetLog: Integer): TIntegerCollection;
  var
    ModulosBigInts: TBigIntCollection;
    CurrentSet: TBigIntCollection;
    MinSetMask: Integer;
    MinProd, Temp: TBigInt;
    i, j: Integer;
    mCount: Integer;

  begin
    ModulosBigInts := TBigIntCollection.Create;
    for i := 0 to Modulos.Size - 1 do
    begin
      WriteLn('M[', i, '] = ', Modulos[i]);
      ModulosBigInts.Add(
          BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(Modulos[i]).Decr);
    end;

    mCount := Modulos.Size;

    MinProd := BigIntFactory.ComputeProduct(ModulosBigInts);
    Assert(TargetLog <= MinProd.Log, 'TargetLog = ' + IntToStr(TargetLog) +
      ' all modulos prod = ' + IntToStr(MinProd.Log));
    MinSetMask := (1 shl mCount) - 1;

    CurrentSet := TBigIntCollection.Create;
    for i := 0 to ((1 shl mCount) - 1) do
    begin
      CurrentSet.Clear;

      for j := 0 to mCount - 1 do
        if (i and (1 shl j)) <> 0 then
          CurrentSet.Add(ModulosBigInts[j]);

      Temp := BigIntFactory.ComputeProduct(CurrentSet);
      if Temp.Log < TargetLog then
      else if MinProd.CompareWith(Temp) > 0 then
      begin
        BigIntFactory.ReleaseMember(MinProd);
        MinProd := Temp.Copy;
        MinSetMask := i;
      end;

      BigIntFactory.ReleaseMember(Temp);
    end;
    CurrentSet.Free;

    for i := 0 to ModulosBigInts.Count - 1 do
      BigIntFactory.ReleaseMember(ModulosBigInts[i]);
    ModulosBigInts.Clear;
    ModulosBigInts.Free;

    Result := TIntegerCollection.Create;
    for j := 0 to mCount - 1 do
      if (MinSetMask and (1 shl j)) <> 0 then
        Result.PushBack(Modulos[j]);
  end;

var
  {Modulos[i] * AllPrimes[i] = ModulosPlus_1[i]}
  AllPrimes, Modulos: TIntegerCollection;
  Values: TBigIntCollection;
  b: Integer;
  logn, nOverLogn: Integer;
  p, PCount: Integer;
  i, j: Integer;

  Prod, Temp, t2p: TBigInt;
  MaxModulo: Integer;

begin
  if MaxModLogVal = -1 then
    MaxModLogVal := MaxInt;

  AllPrimes := GenerateAllPrimesLessThanEq(n div 2);

  logn := Round(log2(n));
  nOverLogn := (n + logn - 1) div logn;

  if (GetRunTimeParameterManager.Verbosity and
                  VerbModuloBasedBinaryArithmeticCircuit) <> 0 then
    WriteLn('logn = ', logn);

  PCount := AllPrimes.Size;
  Modulos := TIntegerCollection.Create;

  Prod := BigIntFactory.GetNewMember.SetValue(1);
  MaxModulo := nOverLogn div 2;
  while True do
  begin
    if n / 2 < MaxModulo then
      MaxModulo:= n div 2 - 1;

    for i := 0 to Min(PCount - 1, logn) do
    begin
      Modulos.PushBack(AllPrimes[i]);
      p := AllPrimes[i];
//      WriteLn('p =', p);

      while Modulos[i] <= MaxModulo do
        Modulos[i] := Modulos[i] * p;
      if n / 2 <= Modulos[i]  then
        Modulos[i] := Modulos[i] div p;

//      WriteLn('M[', i, '] = ', Modulos[i], ' ', nOverLogn);

      t2p := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(Modulos[i]).Decr;
      assert(not t2p.IsZero);

      Temp := Prod.Mul(t2p);
      BigIntFactory.ReleaseMember(t2p);
      BigIntFactory.ReleaseMember(Prod);
      Prod := Temp;
    end;

    if n < Prod.Log then
      Break;

    MaxModulo *= 2;
    Inc(logn);
  end;
  AllPrimes.Free;


  Result := SelectSmallestSubset(Modulos, n);
  Modulos.Free;

end;

function TModuloBasedBinaryArithmeticCircuit.GenerateModulos3(n: Integer
  ): TIntegerCollection;

  function SelectSmallestSubset(Modulos: TIntegerCollection;
    TargetLog: Integer): TIntegerCollection;
  var
    ModulosBigInts: TBigIntCollection;
    CurrentSet: TBigIntCollection;
    MinSetMask: Integer;
    MinProd, Temp: TBigInt;
    i, j: Integer;
    mCount: Integer;

  begin
    ModulosBigInts := TBigIntCollection.Create;
    for i := 0 to Modulos.Size - 1 do
    begin
      ModulosBigInts.Add(
          BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(Modulos[i]).Decr);
      WriteLn('M[', i, '] = ', Modulos[i], ' ', ModulosBigInts[i].ToString);
    end;

    mCount := Modulos.Size;

    MinProd := BigIntFactory.ComputeProduct(ModulosBigInts);
    Assert(TargetLog <= MinProd.Log, 'TargetLog = ' + IntToStr(TargetLog) +
      ' all modulos prod = ' + IntToStr(MinProd.Log));
    MinSetMask := (1 shl mCount) - 1;

    CurrentSet := TBigIntCollection.Create;
    for i := 0 to ((1 shl mCount) - 1) do
    begin
      CurrentSet.Clear;

      for j := 0 to mCount - 1 do
        if (i and (1 shl j)) <> 0 then
          CurrentSet.Add(ModulosBigInts[j]);

      Temp := BigIntFactory.ComputeProduct(CurrentSet);
      if Temp.Log < TargetLog then
      else if MinProd.CompareWith(Temp) > 0 then
      begin
        BigIntFactory.ReleaseMember(MinProd);
        MinProd := Temp.Copy;
        MinSetMask := i;
      end;

      BigIntFactory.ReleaseMember(Temp);
    end;
    CurrentSet.Free;

    for i := 0 to ModulosBigInts.Count - 1 do
      BigIntFactory.ReleaseMember(ModulosBigInts[i]);
    ModulosBigInts.Clear;
    ModulosBigInts.Free;

    Result := TIntegerCollection.Create;
    for j := 0 to mCount - 1 do
      if (MinSetMask and (1 shl j)) <> 0 then
        Result.PushBack(Modulos[j]);
  end;

var
  i, j: Integer;
  ActivePrime: Integer;
  logn: Integer;
  Temp : TBigIntCollection;
  Prod, t2p: TBigInt;
  Modulos, AllPrimes: TIntegerCollection;

begin
  Modulos := TIntegerCollection.Create;
  AllPrimes := GenerateAllPrimesLessThanEq(n div 2);

  logn := Round(log2(n));

  Temp := TBigIntCollection.Create;

  for i := 0 to AllPrimes.Size - logn do
  begin
    for j := 0 to Temp.Count - 1 do
      BigIntFactory.ReleaseMember(Temp[j]);

    Temp.Clear;
    Modulos.Clear;
    for j := 1 to logn do
    begin
      ActivePrime := AllPrimes[i + j - 1];
      if n  div 2 <= ActivePrime then
        break;

{      while (ActivePrime * AllPrimes[i + j - 1]) < (n div 3) do
        ActivePrime *= AllPrimes[i + j - 1];
}
      Modulos.PushBack(ActivePrime);
      t2p := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(ActivePrime).Decr;
      Temp.Add(t2p);

      Prod := BigIntFactory.ComputeProduct(Temp);

      if n < Prod.Log then
        break;

      BigIntFactory.ReleaseMember(Prod);
      Prod := nil;
    end;

    if Prod <> nil then
    begin
//      BigIntFactory.ReleaseMemeber(Prod);
      Prod := nil;
      Break;
    end;
  end;

  assert(Prod = nil);
//  for j := 0 to Temp.Count - 1 do
//    BigIntFactory.ReleaseMemeber(Temp[i]);
  Temp.Clear;
  Temp.Free;

  Result := SelectSmallestSubset(Modulos, n);
  Modulos.Free;

end;

function TModuloBasedBinaryArithmeticCircuit.GenerateModulos4(n: Integer
  ): TIntegerCollection;
  function IsPossible(AllPrimes: TIntegerCollection; MaxVal: Integer;
    ModuloCount: Integer): TIntegerCollection;
  var
    i, j: Integer;
    AllModulos: TIntegerCollection;
    ActivePrime: Integer;
    MaxIndex, Max: Integer;

  begin
    Result := TIntegerCollection.Create;

    AllModulos := TIntegerCollection.Create;

    for i := 0 to AllPrimes.Size - 1 do
    begin
      ActivePrime := AllPrimes[i];

      if ActivePrime < MaxVal then
      begin
        while ActivePrime * AllPrimes[i] < MaxVal do
          ActivePrime *= AllPrimes[i];

        AllModulos.PushBack(ActivePrime);
      end;
    end;

    for i := 1 to ModuloCount do
    begin
      Max := -1;
      MaxIndex := -1;

      for j := 0 to AllModulos.Size - 1 do
        if Max < AllModulos[j] then
        begin
          MaxIndex := j;
          Max := AllModulos[j];
        end;
      AllModulos.Erase(MaxIndex);
      Result.PushBack(Max);
    end;

    AllModulos.Free;

  end;

var
  Top, Bot, Mid, Last: Integer;
  OrigAllPrimes, CurrentModuloSet: TIntegerCollection;
  Prod: TBigInt;
  logn: Integer;
  i, j: Integer;
  TwoToN : TBigInt;
  MinMax: Integer;

begin
  TwoToN := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(n);

  OrigAllPrimes := GenerateAllPrimesLessThanEq(n div 2);

  logn := Round(log2(n));
  Last := n div 2;

  Result := nil;
  for i := 1 to logn do
  begin
    Top := Last - 1;
    Bot := 1;

    while Bot <= Top do
    begin
      Mid := (Bot + Top) div 2;

      CurrentModuloSet := IsPossible(OrigAllPrimes, Mid, i);
      Prod := ComputeModuloProduct(CurrentModuloSet);
      if 0 <= Prod.CompareWith(TwoToN) then
      begin
        Result.Free;
        Result := TIntegerCollection.Create(CurrentModuloSet);

        Last := Mid;
        Top := Mid - 1;
      end
      else
        Bot := Mid + 1;

      BigIntFactory.ReleaseMember(Prod);
      CurrentModuloSet.Free;
    end;
  end;

  OrigAllPrimes.Free;
  BigIntFactory.ReleaseMember(TwoToN);

  for i := 0 to Result.Size - 1 do
    Write(Result[i], ' ');
  WriteLn;

  CurrentModuloSet := SelectSmallestSubset(Result, n);
  Result.Free;

  Result := CurrentModuloSet;
end;

function TModuloBasedBinaryArithmeticCircuit.GenerateModulos5(n: Integer
  ): TIntegerCollection;
  function IsPossible(AllPrimes: TIntegerCollection; MaxVal: Integer;
    ModuloCount: Integer): TIntegerCollection;
  var
    i, j: Integer;
    AllModulos: TIntegerCollection;
    ActivePrime: Integer;
    MaxIndex, Max: Integer;

  begin
    Result := TIntegerCollection.Create;

    AllModulos := TIntegerCollection.Create;

    for i := 0 to AllPrimes.Size - 1 do
    begin
      ActivePrime := AllPrimes[i];

      if ActivePrime < MaxVal then
        AllModulos.PushBack(ActivePrime);
    end;

    for i := 1 to ModuloCount do
    begin
      Max := -1;
      MaxIndex := -1;

      for j := 0 to AllModulos.Size - 1 do
        if Max < AllModulos[j] then
        begin
          MaxIndex := j;
          Max := AllModulos[j];
        end;
      AllModulos.Erase(MaxIndex);
      Result.PushBack(Max);
    end;

    AllModulos.Free;

  end;

var
  Top, Bot, Mid, Last: Integer;
  OrigAllPrimes, CurrentModuloSet: TIntegerCollection;
  Prod: TBigInt;
  logn: Integer;
  i, j: Integer;
  TwoToN : TBigInt;
  MinMax: Integer;

begin
  TwoToN := BigIntFactory.GetNewMember.SetValue(1).ShiftLeft(n);

  OrigAllPrimes := GenerateAllPrimesLessThanEq(n div 2);

  logn := Round(log2(n));
  Last := n div 2;

  Result := nil;
  for i := 1 to logn do
  begin
    Top := Last - 1;
    Bot := 1;

    while Bot <= Top do
    begin
      Mid := (Bot + Top) div 2;

      CurrentModuloSet := IsPossible(OrigAllPrimes, Mid, i);
      Prod := ComputeModuloProduct(CurrentModuloSet);
      if 0 <= Prod.CompareWith(TwoToN) then
      begin
        Result.Free;
        Result := TIntegerCollection.Create(CurrentModuloSet);

        Last := Mid;
        Top := Mid - 1;
      end
      else
        Bot := Mid + 1;

      BigIntFactory.ReleaseMember(Prod);
      CurrentModuloSet.Free;
    end;
  end;

  OrigAllPrimes.Free;
  BigIntFactory.ReleaseMember(TwoToN);

  for i := 0 to Result.Size - 1 do
    Write(Result[i], ' ');
  WriteLn;

  CurrentModuloSet := SelectSmallestSubset(Result, n);
  Result.Free;

  Result := CurrentModuloSet;
end;

function TModuloBasedBinaryArithmeticCircuit.GenerateModulos6(n: Integer
  ): TIntegerCollection;
var
  pCount: Integer;
  OrigAllPrimes: TIntegerCollection;

begin
  PCount := 2;
  OrigAllPrimes := GenerateAllPrimesLessThanEq(n div 2);

  OrigAllPrimes.Free;
  //
end;

function TModuloBasedBinaryArithmeticCircuit.GenerateAllPrimesLessThanEq(
  Last: Integer): TIntegerCollection;
var
  IsPrime: array of Boolean;
  i, j: Integer;

begin
  SetLength(IsPrime, Last + 1);

  for i := 0 to Last do
    IsPrime[i] := True;

  IsPrime[0] := False;
  IsPrime[1] := False;
  for i := 2 to (Last + 1) div 2 do
    if IsPrime[i] then
      for j := 2 to Last  div i do
        IsPrime[j * i] := False;

  Result := TIntegerCollection.Create;
  for i := 2 to Last do
    if IsPrime[i] then
      Result.PushBack(i);

end;

function TModuloBasedBinaryArithmeticCircuit.GenerateModulosUsingDP(n: Integer
  ): TIntegerCollection;
type
  TPair = record
    Sum: TIntegerCollection;
    Count: Integer;
  end;

var
  AllPrimes: TIntegerCollection;
  dp: array of TIntegerCollection;
  logn: Integer;
  i, j: Integer;
  TargetSum: Integer;
  ActivePrime: Integer;

begin
  AllPrimes := GenerateAllPrimesLessThanEq(2 * n);

  logn := Round(log2(n / 2.0)) + 1;
  SetLength(dp, logn + 1);
  for i := 0 to logn  do
  begin
    dp[i] := TIntegerCollection.Create;
    dp[i].Resize(3 * n);
    for j := 0 to dp[i].Size - 1 do
      dp[i][j] := 0;
  end;


  dp[0][0] := 1;
  dp[0][AllPrimes[0]] := 1;
  for i := 1 to AllPrimes.Size - 1 do
  begin
    ActivePrime := AllPrimes[i];

    for j := 0 to dp[i - 1].Size - 1 do
      if dp[i - 1][j] <> 0 then
      begin
        dp[i][j] := 1;
        if j + ActivePrime < dp[i].Size then
          dp[i][j + ActivePrime] := 1;
      end;
  end;

  TargetSum := -1;
  for i := 2 * n + 3 to dp[AllPrimes.Size - 1].Size do
    if dp[AllPrimes.Size - 1][i] <> 0 then
    begin
      TargetSum := i;
      break;
    end;
  assert(TargetSum <> -1, 'TargetSum = -1');

  Result := TIntegerCollection.Create;
  i := AllPrimes.Size - 1;
  while 1 <= i do
  begin
    Assert(dp[i][TargetSum] <> -1);
    ActivePrime := AllPrimes[i];

    if dp[i - 1][TargetSum] <> 0 then
    else if dp[i - 1][TargetSum - ActivePrime] <> 0  then
    begin
      Result.PushBack(ActivePrime);
      TargetSum -= ActivePrime;
    end
    else
      Assert(False,
        'both dp[i-1][TargetSum] and dp[i-1][TargetSum-ActivePrime] are False!');

    Dec(i);
  end;

  assert((TargetSum = 2) or (TargetSum = 0));
  if TargetSum = 2 then
    Result.PushBack(2);

  for i := 0 to logn  do
    dp[i].Free;
  SetLength(dp, 0);
  AllPrimes.Free;
end;

var
  last_n: Integer;

procedure dWriteLn(msg: String = ''; n: Integer = -1);
begin
  if (GetRunTimeParameterManager.Verbosity and
       VerbModuloBasedBinaryArithmeticCircuit) <> 0 then
  begin
    if n = -1 then
      n := GetVariableManager.LastUsedCNFIndex;

    if msg <> '' then
      WriteLn(msg + ' : ', n - last_n);
    last_n := n;
  end;
end;

function TModuloBasedBinaryArithmeticCircuit.EncodeMul(const a, b,
  c: TBitVector; Level: Integer): TLiteral;
{
  This method selects Log(n) prime integers, p1,...,p_m, such that their summation is
  greater than 2n + 2, and returns 2^p_i - 1, for i = 1, ..., m = Log(n).
}

  procedure AddExtraClausesForModulo(a, r: TBitVector; m: Integer);
  var
    b, bShiftLeftBym: TBitVector;
    LHS, RHS: TBitVector;
    i: Integer;

  begin // r = a % (2^m - 1) => a - r = b(2^m - 1)
    { a + b = b * 2^m + r }
    b := TBitVector.Create(a.Count - r.Count + 1);
    LHS := Add(a, b);
    bShiftLeftBym := b.Copy;
    bShiftLeftBym.Resize(bShiftLeftBym.Count + m);
    for i := b.Count - 1 downto 0 do
      bShiftLeftBym[i + m] := bShiftLeftBym[i];
    for i := 0 to m - 1 do
      bShiftLeftBym[i] := GetVariableManager.FalseLiteral;
    RHS := Add(bShiftLeftBym, r);

    if (StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
       VerbModulo_ModuloBasedBinaryArithmeticCircuit)<> 0 then
    begin
      WriteLn('[TModuloBasedBinaryArithmeticCircuit.Modulo] a = ', a.ToString);
      WriteLn('[TModuloBasedBinaryArithmeticCircuit.Modulo] b = ', b.ToString);
      WriteLn('[TModuloBasedBinaryArithmeticCircuit.Modulo] LHS = ', LHS.ToString);
      WriteLn('[TModuloBasedBinaryArithmeticCircuit.Modulo] bShiftLeftBym = ', bShiftLeftBym.ToString);
      WriteLn('[TModuloBasedBinaryArithmeticCircuit.Modulo] r = ', r.ToString);
      WriteLn('[TModuloBasedBinaryArithmeticCircuit.Modulo] RHS = ', RHS.ToString);
    end;
    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(EncodeIsEqual(LHS, RHS));
    SatSolver.SubmitClause;

    b.Free;
    bShiftLeftBym.Free;
    RHS.Free;
    LHS.Free;

  end;

var
  i: Integer;
  m: Int64;
  Modulos: TIntegerCollection;
  aModm, bModm, cModm, TempVector: TBitVector;
  aModbMod, aModbModModm: TBitVector;
  Prod: TBigInt;
  TotalVar: Integer;

begin
{  WriteLn('<EncodeMul aCount= "', a.Count, '" bCount= "', b.Count, '">');
}
  assert(a.Count + b.Count <= c.Count);

  TotalVar:= GetVariableManager.LastUsedCNFIndex;
  if {(a.Count + b.Count <= 200) or} (0 < level)
         then
  begin
    TempVector := inherited Mul(a, b);
    Result := EncodeIsEqual(TempVector, c);
    TempVector.Free;
    Exit;
  end;
  if (GetRunTimeParameterManager.Verbosity and
               VerbModuloBasedBinaryArithmeticCircuit) <> 0 then
  begin
    WriteLn('EncodeMul');
    WriteLn('a = ' + a.ToString);
    WriteLn('b = ' + b.ToString);
    WriteLn('c = ' + c.ToString);
  end;

//  WriteLn('Mul a.Count = ', a.Count, ' b.Count = ', b.Count);
  Modulos := nil;
  if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModuloMode'))
         = UpperCase('dp') then
    Modulos := GenerateModulosUsingDP(a.Count + b.Count)
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModuloMode'))
         = UpperCase('Mode1') then
    Modulos := GenerateModulos1(a.Count + b.Count, Min(a.Count, b.Count))
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModuloMode'))
                = UpperCase('Mode2') then
           Modulos := GenerateModulos2(a.Count + b.Count, Min(a.Count, b.Count))
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModuloMode'))
         = UpperCase('Mode3') then
    Modulos := GenerateModulos3(a.Count + b.Count)
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModuloMode'))
                = UpperCase('Mode4') then
    Modulos := GenerateModulos4(a.Count + b.Count)
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModuloMode'))
         = UpperCase('Mode5') then
    Modulos := GenerateModulos5(a.Count + b.Count)
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--ModuloMode'))
         = UpperCase('Mode6') then
    Modulos := GenerateModulos6(a.Count + b.Count);

  assert(Modulos <> nil);

  Prod := ComputeModuloProduct(Modulos);
  if (GetRunTimeParameterManager.Verbosity and
         VerbModuloBasedBinaryArithmeticCircuit) <> 0 then
    for i := 0 to Modulos.Size - 1 do
      WriteLn('Modulo[ ', i, '] = 2^', Modulos[i]);

  Assert(a.Count + b.Count <= Prod.Log);
  BigIntFactory.ReleaseMember(Prod);

  Write(a.Count + b.Count, ':');
  for i:= 0 to Modulos.Size- 1 do
    Write(' ', Modulos[i]);
  WriteLn;

  SatSolver.BeginConstraint;
  for i:= 0 to Modulos.Size- 1 do
  begin
    m := Modulos[i];

    assert(m < Max(a.Count, b.Count), 'm  = '+ IntToStr(m) + ' a.Count = ' + IntToStr(a.Count) +  ' b.Count = ' + IntToStr(b.Count));

    if (GetRunTimeParameterManager.Verbosity and
                  VerbModuloBasedBinaryArithmeticCircuit) <> 0 then
    begin
      WriteLn('[TModuloBasedBinaryArithmeticCircuit]: M [', i, '] = 2^', m, ' - 1 mBin= ', IntToStr(m));
    end;

    dWriteLn();
    aModm := Modulo(a, m);
    dWriteLn('amod ' + IntToStr(a.Count));
    SatSolver.AddComment('a and amodM = ' + a.ToString + ' ' + aModm.ToString);

    SatSolver.AddLiteral(GetVariableManager.TrueLiteral);

    bModm := Modulo(b, m);
    dWriteLn('bmod ' + IntToStr(b.Count));
    SatSolver.AddComment('b and bmodM = ' + b.ToString + ' ' + bModm.ToString);

    cModm := Modulo(c, m);
    dWriteLn('cmod ' + IntToStr(b.Count));
    SatSolver.AddComment('c and cmodM = ' + c.ToString + ' ' + cModm.ToString);

    aModbMod:= TBitVector.Create(aModm.Count + bModm.Count);
    dWriteLn('defining amodbmod ' + IntToStr(aModbMod.Count));

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(EncodeMul(aModm, bModm, aModbMod, level + 1));
    SatSolver.SubmitClause;
    dWriteLn('EncodeMul ');

    aModbModModm := Modulo(aModbMod, m);
    dWriteLn('aModbModModm ' + IntToStr(m));

    SatSolver.AddLiteral(Self.EncodeIsEqual(cModm, aModbModModm));
    dWriteLn('EncodeIsEqual ' + IntToStr(m));


    if (i = 0) and
    (Pos(UpperCase('Modulo_1:'),
      UpperCase(GetRunTimeParameterManager.ValueByName['--ExtraClausesLevel'])) <> 0) then
    begin
      AddExtraClausesForModulo(c, cModm, m);
      AddExtraClausesForModulo(b, bModm, m);
      AddExtraClausesForModulo(a, aModm, m);
      AddExtraClausesForModulo(aModbMod, aModbModModm, m);
    end
    else if Pos(UpperCase('Modulo_2:'),
      UpperCase(GetRunTimeParameterManager.ValueByName['--ExtraClausesLevel'])) <> 0 then
    begin
      AddExtraClausesForModulo(c, cModm, m);
      AddExtraClausesForModulo(b, bModm, m);
      AddExtraClausesForModulo(a, aModm, m);
      AddExtraClausesForModulo(aModbMod, aModbModModm, m);
    end;

    if (GetRunTimeParameterManager.Verbosity and
                 VerbModuloBasedBinaryArithmeticCircuit) <> 0 then
    begin
      WriteLn('[TModuloBasedBinaryArithmeticCircuit]: M [', i, '] = 2^', m, ' - 1 mBin= ', IntToStr(m));
      WriteLn('[TModuloBasedBinaryArithmeticCircuit]: a = ' + a.ToString);
      WriteLn('[TModuloBasedBinaryArithmeticCircuit]: aMod = ' + aModm.ToString);
      WriteLn('[TModuloBasedBinaryArithmeticCircuit]: b = ' + b.ToString);
      WriteLn('[TModuloBasedBinaryArithmeticCircuit]: bMod = ' + bModm.ToString);
      WriteLn('[TModuloBasedBinaryArithmeticCircuit]: c = ' + c.ToString);
      WriteLn('[TModuloBasedBinaryArithmeticCircuit]: cMod = ' + cModm.ToString);
      WriteLn('[TModuloBasedBinaryArithmeticCircuit]: aModbMod = ' + aModbMod.ToString);
      WriteLn('[TModuloBasedBinaryArithmeticCircuit]: aModbModModm = ' + aModbModModm.ToString);
      WriteLn('[TModuloBasedBinaryArithmeticCircuit]: M [', i, '] = 2^', m, ' - 1 mBin= ', IntToStr(m));
    end;

    aModm.Free;
    bModm.Free;
    cModm.Free;
    aModbMod.Free;
    aModbModModm.Free;
  end;

  Modulos.Free;

  Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);
  SatSolver.SubmitAndGate(Result);

end;

function TModuloBasedBinaryArithmeticCircuit.Modulo(const a: TBitVector;
  const m: Int64): TBitVector;
type
    TBitVectorList= specialize TVector<TBitVector>;

  function FindConjunction(const vecList: TBitVector): TLiteral;
  var
    i: Integer;
  begin
    SatSolver.BeginConstraint;

    for i := 0 to vecList.Count - 1 do
      SatSolver.AddLiteral(vecList[i]);
    Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);
    SatSolver.SubmitAndGate(Result);
  end;

  function ParallelAdder(const Mat: TBitVectorList; Start, Finish: Integer): TBitVector;
  var
    FirstHalf, SecondHalf: TBitVector;

  begin
    if Start = Finish then
    begin
      Result := Mat[Start].Copy;

      if (StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
          VerbModulo_ModuloBasedBinaryArithmeticCircuit)<> 0 then
        WriteLn('[Mul.ParallelAdder] Mat[', Start, '] = ', Result.ToString);

      Exit;
    end;

    FirstHalf := ParallelAdder(Mat, Start, (Start + Finish) div 2);
    SecondHalf := ParallelAdder(Mat, (Start + Finish) div 2 + 1, Finish);
    if StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
     (1 shl VerbModulo_ModuloBasedBinaryArithmeticCircuit)<> 0 then
    begin
      WriteLn('[Mul.ParallelAdder] FirstHalf = ', FirstHalf.ToString);
      WriteLn('[Mul.ParallelAdder] SecondHalf = ', SecondHalf.ToString);
    end;

    Result := AddModuloMod(FirstHalf, SecondHalf, m);

    if (StrToInt(GetRunTimeParameterManager.ValueByName['--Verbosity']) and
       VerbModulo_ModuloBasedBinaryArithmeticCircuit)<> 0 then
    begin
      WriteLn('[Mul.ParallelAdder] Mat[', Start, '] + ...+  Mat[', Finish, '] = ', Result.ToString);
    end;

    FirstHalf.Free;
    SecondHalf.Free;

  end;

var
  Mat: TBitVectorList;
  Current: TBitVector;
  i: Integer;
  b, bShiftLeftBym, RHS, LHS: TBitVector;

begin
  if a.Count <= m then
  begin// special case
    Result := a.Copy;
    for i := a.Count to m - 1 do
      Result.PushBack(GetVariableManager.FalseLiteral);
    Exit;
  end;

  Mat := TBitVectorList.Create;

  for i := 0 to a.Count - 1 do
  begin
    if i mod m = 0 then
    begin
      Current := TBitVector.Create;//(m, GetVariableManager.FalseLiteral);
      Mat.PushBack(Current);
    end;
    Current.Add(a[i]);
  end;

  for i := Current.Count + 1 to m do
    Current.PushBack(GetVariableManager.FalseLiteral);

  Result := ParallelAdder(Mat, 0, Mat.Size - 1);
  Mat.Free;

end;

function TModuloBasedBinaryArithmeticCircuit.IncByTwoToM_1(const a: TBitVector;
  const m: Int64): TBitVector;
var
  TwoToMMinusOne: TBitVector;
  i, j: Integer;

begin
  assert(a.Count = m);

  TwoToMMinusOne := TBitVector.Create(m, GetVariableManager.TrueLiteral);
  Result := Add(a, TwoToMMinusOne);
  assert(Result.Count = m + 1);
  TwoToMMinusOne.Free;

  for i := 0 to a.Count - 1 do
    for j := i + 1 to a.Count - 1 do
    begin
      // ai & aj -> Result[j]
      // ~ai, ~aj, rj

      if Pos('IncByTwoToM_1_1', GetRunTimeParameterManager.ValueByName['--ExtraClausesMode'])
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
      if Pos('IncByTwoToM_1_2', GetRunTimeParameterManager.ValueByName['--ExtraClausesMode'])
              <> 0 then
      begin
        SatSolver.BeginConstraint;
        SatSolver.AddLiterals([NegateLiteral(a[i]),
                               a[j],
                               NegateLiteral(Result[j])]);
        SatSolver.SubmitClause;
      end;
    end;

end;

function TModuloBasedBinaryArithmeticCircuit.EncodeGreaterThan(
  const a: TBitVector; const m: Int64): TLiteral;
var
  i: Integer;
begin
  Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);
  SatSolver.AddComment('GreaterThan ' + a.ToString + ' ' + IntToStr(m) + ' <=> ' + LiteralToString(Result));

  SatSolver.BeginConstraint;

  for i := m to a.Count - 1 do
    SatSolver.AddLiteral(a[i]);
  SatSolver.SubmitOrGate(Result);

  SatSolver.AddComment('GreaterThan ' + a.ToString + ' ' + IntToStr(m) + ' <=> ' + LiteralToString(Result));

end;

function TModuloBasedBinaryArithmeticCircuit.EncodeAddModuloMod(const a, b,
  c: TBitVector; m: Integer): TLiteral;
var
  Sum, TwoToMMinusOne, CPlusTwoTpMMinusOne: TBitVector;
  i: Integer;
  l, l1, l2: TLiteral;

begin
  Assert(a.Count = m);
  Assert(b.Count = m);
  Assert(m <= c.Count);

  for i := m to c.Count - 1 do
  begin
    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(NegateLiteral(c[i]));
    SatSolver.SubmitClause;
  end;

  SatSolver.BeginConstraint;
  for i := 0 to c.Count - 1 do
    SatSolver.AddLiteral(NegateLiteral(c[i]));
  SatSolver.SubmitClause;

  Sum := Add(a, b);
  l1 := EncodeIsEqual(c, Sum);

  if (Sum.Count = m) or (SatSolver.GetLiteralValue(Sum[m]) = gbFalse) then // 0..m-1  2^m-1
    Result := l1
  else
  begin
    TwoToMMinusOne := TBitVector.Create(m, GetVariableManager.TrueLiteral);
    CPlusTwoTpMMinusOne := Add(c, TwoToMMinusOne);
  //    WriteLn('EncodeAddModuloMod: CPlusTwoTpMMinusOne = ' +
  //             CPlusTwoTpMMinusOne.ToString);
    l2 := EncodeIsEqual(CPlusTwoTpMMinusOne, Sum);
   //   WriteLn('EncodeAddModuloMod: l1 = ' + LiteralToString(l1));
   //   WriteLn('EncodeAddModuloMod: l2 = ' + LiteralToString(l2));
{    end;
 }
    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(l1);
    SatSolver.AddLiteral(l2);
     Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);
    SatSolver.SubmitOrGate(Result);

    SatSolver.BeginConstraint;
    SatSolver.AddLiteral(l1);
    SatSolver.AddLiteral(l2);
    SatSolver.SubmitXOrGate(GetVariableManager.TrueLiteral);// ! (l1 -> ~l2) and (l2 -> ~l1)

  end;
//  WriteLn('EncodeAddModuloMod: Result = ', LiteralToString(Result));
  SatSolver.AddComment(a.ToString + ' + ' + b.ToString + ' = ' + c.ToString + ' modulo 2^'+ IntToStr(m)+ '- 1');

end;

function TModuloBasedBinaryArithmeticCircuit.AddModuloMod(const a,
  b: TBitVector; m: Integer): TBitVector;

  function IsAllKnown(a: TBitVector): Boolean;
  var
    i: Integer;

  begin
    for i := 0 to a.Count - 1 do
      if SatSolver.GetLiteralValue(a[i]) = gbUnknown then
        Exit(False);

    Result := True;
  end;

  function IsLessThanMForSure(const a: TBitVector): Boolean;
  var
    i: Integer;

  begin
    //assert(a.Count <= m + 1);
    for i := m to a.Count - 1 do
      if SatSolver.GetLiteralValue(a[i]) <> gbFalse then
        Exit(False);

    for i := 0 to m - 1 do
      if SatSolver.GetLiteralValue(a[i]) = gbFalse then
        Exit(True);

    Result := False;
  end;

  function IsEqMForSure(const a: TBitVector): Boolean;
  var
    i: Integer;

  begin
    for i := m to a.Count - 1 do
      if SatSolver.GetLiteralValue(a[i]) <> gbFalse then
        Exit(False);
    for i := 0 to m - 1 do
      if SatSolver.GetLiteralValue(a[i]) = gbFalse then
        Exit(False);

    Result := True;
  end;

  function IsNotEqMForSure(const a: TBitVector): Boolean;
  var
    i: Integer;

  begin
    for i := m to a.Count - 1 do
      if SatSolver.GetLiteralValue(a[i]) = gbTrue then
        Exit(True);

    for i := 0 to m - 1 do
      if SatSolver.GetLiteralValue(a[i]) = gbFalse then
        Exit(True);

    Result := False;
  end;

var
  Sum, RPlusM, One: TBitVector;
  TotalVar: Integer;
  i, j, k: Integer;
  Li: array [0..2] of TLiteral;
  ClausesForLi: array [0..2] of TClauseCollection;

begin
  TotalVar := GetVariableManager.LastUsedCNFIndex;

  Assert(a.Count = m);
  Assert(b.Count = m);
  RPlusM := nil;

  Sum := Add(a, b);
  Li[0] := GetVariableManager.FalseLiteral;// S < M => R = S
  Li[1] := Li[1];                          // S = M => R = 0
  Li[2] := Li[2];                          // S > M  => R + M = S
  One := TBitVector.Create(m, GetVariableManager.TrueLiteral); // One = M
  if IsLessThanMForSure(Sum) then // R = S
    Result := Sum.Copy
  else if IsEqMForSure(Sum) then // R = 0
    Result := TBitVector.Create(m, GetVariableManager.FalseLiteral)
  else if IsAllKnown(Sum) then // Sum > M
  begin
    Result := Incr(Sum);
    Result.Count := m;
  end
  else
  begin
    Li[2] := Sum[m];
    if IsNotEqMForSure(Sum) then
      Li[1] := GetVariableManager.FalseLiteral
    else
      Li[1] := EncodeIsEqual(Sum, One);

    SatSolver.BeginConstraint;
    SatSolver.AddLiterals([Li[1], Li[2]]);
    Li[0] := NegateLiteral(SatSolver.GenerateOrGate);

    SatSolver.SubmitExactlyOne(Li);

    Result := TBitVector.Create(m);

    ReNewSatSolver('CNFCollection');
    SubmitIsEqual(Result, Sum, GetVariableManager.TrueLiteral);
    ClausesForLi[0] := SatSolver.CNF.Copy;
    PopBackSatSolver;

    ReNewSatSolver('CNFCollection');
    for i := 0 to Result.Count - 1 do
    begin
      SatSolver.BeginConstraint;
      SatSolver.AddLiteral(NegateLiteral(Result[i]));
      SatSolver.SubmitClause;
    end;
    ClausesForLi[1] := SatSolver.CNF.Copy;
    PopBackSatSolver;

    ReNewSatSolver('CNFCollection');
    if GetRunTimeParameterManager.ValueByName['--AddModuloMod'] = 'Decr' then
    begin // (R - 1) + 2^m
      RPlusM := Decr(Result);
      for i := RPlusM.Count to m - 1 do
       RPlusM.PushBack(GetVariableManager.FalseLiteral);
      RPlusM.PushBack(GetVariableManager.TrueLiteral);
    end
    else if GetRunTimeParameterManager.ValueByName['--AddModuloMod'] = 'IncByTwoToM_1' then
    begin // R + (2^M - 1)
      RPlusM := IncByTwoToM_1(Result, m);
    end
    else
      assert(False);
    SubmitIsEqual(RPlusM, Sum, GetVariableManager.TrueLiteral);
    ClausesForLi[2] := SatSolver.CNF.Copy;
    PopBackSatSolver;

    for k := 0 to 2 do
    begin
      for i := 0 to ClausesForLi[k].Size - 1 do
      begin// l1 -> l1Clauses[0] & ... l1 ->l1Clauses[n - 1];
        SatSolver.BeginConstraint;
        SatSolver.AddLiteral(NegateLiteral(Li[k]));
        for j := 0 to ClausesForLi[k][i].Size - 1 do
          SatSolver.AddLiteral(ClausesForLi[k][i][j]);
        SatSolver.SubmitClause;
      end;
      ClausesForLi[k].Free;
    end;

{    One := TBitVector.Create(m, GetVariableManager.TrueLiteral); // One = M
    SubmitIsEqual(Result, One, GetVariableManager.FalseLiteral); { Result != M}
    One.Free;
}

  end;

  if (GetRunTimeParameterManager.Verbosity and
                     VerbModuloBasedBinaryArithmeticCircuit) <> 0 then
  begin
    WriteLn('Result = ', Result.ToString);
    WriteLn('Sum = ', Sum.ToString);
    if RPlusM <> nil then
      WriteLn('R + 2^M -1 = ', RPlusM.ToString);
    WriteLn('l1 = ', LiteralToString(Li[0]));
    WriteLn('l2 = ', LiteralToString(Li[1]));
    WriteLn('l3 = ', LiteralToString(Li[2]));
  end;
  One.Free;
  RPlusM.Free;
  Sum.Free;

end;

function TModuloBasedBinaryArithmeticCircuit.EncodeMod(const a: TBitVector;
  const m: Int64; const c: TBitVector): TLiteral;
var
  cPrime: TBitVector;

begin
  cPrime := Modulo(a, m);

  Result := Self.EncodeIsEqual(cPrime, c);
end;

constructor TModuloBasedBinaryArithmeticCircuit.Create;
begin
  inherited Create;

end;

destructor TModuloBasedBinaryArithmeticCircuit.Destroy;
begin

  inherited Destroy;
end;

constructor TBinaryModuloFactorizer.Create;
begin
  inherited;

  ArithmeticCircuit.Free;
  ArithmeticCircuit := TModuloBasedBinaryArithmeticCircuit.Create;
end;

destructor TBinaryModuloFactorizer.Destroy;
begin
  inherited Destroy;
end;

end.

