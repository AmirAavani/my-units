{$R+}
unit NumberTheoryUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { TIntList }

  TIntList = class(specialize TFPGList<Int64>)
  public
    procedure Print;
    function Sum: Int64;
  end;

  { TFactorizationPair }

  TFactorizationPair = class(TObject)
  private
    FBase: Integer;
    FPower: Integer;
  public
    property Base: Integer read FBase write FBase;
    property Power: Integer read FPower write FPower;

    constructor Create;
    constructor Create(b, p: Integer);

  end;

  TFactorization = class(specialize TFPGObjectList<TFactorizationPair>)
  public
    procedure WriteLn;
  end;


function GenerateAllPrimes(Max: Integer): TIntList;
function IsPrime(n: Integer): Boolean;
function IsPrime(n: Int64; const Primes: TIntList): Boolean;
function Factorize(n: Int64; const Primes: TIntList): TFactorization;
function Factorize(n: Int64): TFactorization;
function SumOfDivisors(const Factorization: TFactorization): Int64;
function NumberOfDivisors(const Factorization: TFactorization): Int64;
function ToBinary(n: Int64): AnsiString;
{
  Returns sqrt(n) iff n is a perfect square
  Returns -1 o.w.
}
function IsPerfectSquare(n: Int64): Int64;
function CNR(n, r: Integer): UInt64;
function CNRMod(n, r: Integer; AllPrimes: TIntList; Modulo: Int64): UInt64;
function CNRModPrime(n, r: Integer; PrimeModulo: Int64; FactModP: TIntList): UInt64;
function GCD(a, b: UInt64): UInt64;
function Pow(a, b: Integer): Int64;
function PowMod(a, b: Int64; Modulo: Int64): Int64;

{Factors is the factorization for n}
function ComputePhi(n: Int64; Factors: TFactorization): Int64;
function GenerateAllDivisors(Factors: TFactorization): TIntList;

{Chinese Remainder Theorem}
function ChineseRemainderTheorem(Modulos, Remainders: TIntList): uInt64;

{ Modular multiplicative inverse}
function ModularMultiplicativeInverse(x, m: Int64): Int64;
function ModularMultiplicativeInverse4Prime(x, p: Int64): Int64;

implementation

function GenerateAllPrimes(Max: Integer): TIntList;
var
  IsPrime: array of Boolean;
  i, j: Integer;

begin
  Result := TIntList.Create;

  SetLength(IsPrime, Max + 1);
  FillChar(IsPrime[0], (Max + 1) * SizeOf(Boolean), True);

  IsPrime[0] := False;
  IsPrime[1] := False;

  for i := 2 to Max do
    if IsPrime[i] then
      for j := 2 to (Max div i) do
        IsPrime[i * j] := False;

  for i := 2 to Max do
    if IsPrime[i] then
      Result.Add(i);

  SetLength(IsPrime, 0);
end;

function IsPrime(n: Integer): Boolean;
var
  i: Integer;

begin
  Result := False;
  for i := 2 to n - 1 do
  begin
    if n mod i = 0 then
      Exit;
    if n < i * i then
      Break;
  end;
  Result := True;

end;

function IsPrime(n: Int64; const Primes: TIntList): Boolean;
var
  Bot, Top, Mid: Integer;
  i: Integer;

begin
  if n < Primes.Last then
  begin
    Bot := 0;
    Top := Primes.Count - 1;

    while Bot <= Top do
    begin
      Mid := (Bot + Top) div 2;
      if Primes[Mid] < n then
        Bot := Mid + 1
      else if n < Primes[Mid] then
        Top := Mid - 1
      else Exit(True);
    end;
    Result := False;
  end
  else
  begin
    for i := 0 to Primes.Count - 1 do
    begin
      if n mod Primes[i] = 0 then
        Exit(False);
      if n / Primes[i] < Primes[i] then
        break;
    end;
    Result := True;
  end;

end;

function Factorize(n: Int64; const Primes: TIntList): TFactorization;
var
  i, b, p: Integer;

begin
  Result := TFactorization.Create;
  // Assert(Primes.Last * Primes.Last >= n);
  for i := 0 to Primes.Count - 1 do
  begin
    b := Primes[i];
    p := 0;

    while n mod b = 0 do
    begin
      Inc(p);
      n := n div b;
    end;

    if p <> 0 then
      Result.Add(TFactorizationPair.Create(b, p));

    if n < b * b then
      Break;
  end;

  if n <> 1 then
    Result.Add(TFactorizationPair.Create(n, 1));

end;

function Factorize(n: Int64): TFactorization;
var
  i, b, p: Integer;
begin
  Result := TFactorization.Create;
  for i := 2 to n do
  begin
    b := i;
    p := 0;

    while n mod b = 0 do
    begin
      Inc(p);
      n := n div b;
    end;

    if p <> 0 then
      Result.Add(TFactorizationPair.Create(b, p));

    if n < b * b then
    begin
      if n <> 1 then
        Result.Add(TFactorizationPair.Create(n, 1));
      break;
    end;

  end;
end;

function SumOfDivisors(const Factorization: TFactorization): Int64;
var
  i, j: Integer;
  p, b: Integer;
  Sum: Int64;

begin
  Result := 1;
  for i := 0 to Factorization.Count - 1 do
  begin
    b := Factorization[i].Base;
    p := Factorization[i].Power;

    Sum := 1;
    for j := 1 to p do
    begin
      Sum += b;
      b *= Factorization[i].Base;
    end;
    Result *= Sum;
  end;

end;

function NumberOfDivisors(const Factorization: TFactorization): Int64;
var
  i: Integer;

begin
  Result := 1;

  for i := 0 to Factorization.Count - 1 do
    Result := Result * Factorization[i].Power;
end;

function ToBinary(n: Int64): AnsiString;
begin
  Result := '';

  while n <> 0 do
  begin
    if (n and 1) = 0 then
      Result += '0'
    else
      Result += '1';
    n := n shr 1;
  end;
end;

function IsPerfectSquare(n: Int64): Int64;
var
  Bot, Top, Mid: Int64;

begin
  if n < 0 then
    Exit(-1);
  Bot := 0;
  Top := n;

  while Bot <= Top do
  begin
    Mid := (Top + Bot) div 2;

    if Sqr(Mid) < n then
      Bot := Mid + 1
    else if n < Sqr(Mid) then
      Top := Mid - 1
    else
      Exit(Mid);
  end;
  Result := -1;

end;

function CNR(n, r: Integer): UInt64;
var
  NVals: array of Integer;
  i, j, k, l: Integer;
begin
  SetLength(NVals, n + 1);
  FillChar(NVals[0], SizeOf(NVals), 0);
  if n < 2 * r then
    r := n - r;

  for i := n - r + 1 to n do
    NVals[i] := 1;

  for i := 1 to r do
  begin
    k := i;
    while k <> 1 do
    begin
      for j := 1 to n do
        if 0 < NVals[j]  then
        begin
          l := gcd(k, j);
          Dec(NVals[j]);
          Inc(NVals[j div l]);
          k := k div l;
          if k = 1 then
            break;
        end;
    end;
  end;

  Result := 1;
  for i := 2 to n do
    Result *= Pow(i, NVals[i]);

  SetLength(NVals, 0);

end;

function CNRMod(n, r: Integer; AllPrimes: TIntList; Modulo: Int64): UInt64;

  function CountPrime(n, pIndex, p: Integer): Integer;
  begin
    p := AllPrimes[pIndex];
    if n < p then
      Exit(0);

    Result := 0;

    while p <= n do
    begin
      n := n div p;
      Result += n;
    end;
  end;


var
  p: Int64;
  i, j, n_r: Integer;

begin
  n_r := n - r;

  Result := 1;
  for i := 0 to AllPrimes.Count - 1 do
  begin
    p := AllPrimes[i];
    if n < p then
      Break;
    j := CountPrime(n, i, p) - CountPrime(r, i, p) - CountPrime(n_r, i, p) ;
    Result *= Pow(p, j);
    Result := Result mod Modulo;
  end;

end;

function CNRModPrime(n, r: Integer; PrimeModulo: Int64; FactModP: TIntList): UInt64;
begin
  Result := FactModP[n];
  if Result = 0 then
    Exit(0);

  Result *= ModularMultiplicativeInverse4Prime(FactModP[r], PrimeModulo);
  Result := Result mod PrimeModulo;
  Result := (Result * ModularMultiplicativeInverse4Prime(FactModP[n - r], PrimeModulo))
    mod PrimeModulo;
end;

function gcd(a, b: UInt64): UInt64;
var
  c: uInt64;

begin
  if a < b then
  begin
    a := a xor b;
    b := a xor b;
    a := a xor b;
  end;

  while a mod b <> 0 do
  begin
    c := a mod b;
    a := b;
    b := c;
  end;

  Result := b;
end;

function Pow(a, b: Integer): Int64;
begin
  if b = 0 then
    Exit(1);
  if b = 1 then
    Exit(a);
  Result := Sqr(Pow(a, b div 2));
  if Odd(b) then
    Result := Result * a;
end;

function PowMod(a, b: Int64; Modulo: Int64): Int64;
begin
  if b = 0 then
    Exit(1);
  if b = 1 then
    Exit(a mod Modulo);
  Result := Sqr(PowMod(a, b div 2, Modulo)) mod Modulo;
  if Odd(b) then
    Result := (Result * a) mod Modulo;
end;

function ComputePhi(n: Int64; Factors: TFactorization): Int64;
var
  i: Integer;
  a: Integer;

begin
  Result := n;

  for i := 0 to Factors.Count - 1 do
  begin
    a := Factors[i].Base;
    Result := Result div a;
    Result *= (a - 1);
  end;
end;

function GenerateAllDivisors(Factors: TFactorization): TIntList;
  procedure RecGen(Index: Integer);
  var
    i, j, c: Integer;
    p, b2p: UInt64;

  begin
    if Index = Factors.Count then
      Exit;

    c := Result.Count;
    p := Factors[Index].Base;
    b2p := p;

    for j := 1 to Factors[Index].Power do
    begin
      for i := 0 to c - 1 do
        Result.Add(Result[i] * b2p);
      b2p *= p;
    end;

    RecGen(Index + 1);
  end;

begin
  Result := TIntList.Create;

  Result.Add(1);

  if 0 < Factors.Count then
    RecGen(0);
end;

function ChineseRemainderTheorem(Modulos, Remainders: TIntList): uInt64;
var
  n: UInt64;
  ms: TIntList;
  i: Integer;
  SumMs: Integer;

begin
  // Result := \sum ri N/ni[(N/ni)^-1]_ni]_N;
  if Modulos.Count <> Remainders.Count then
    Halt(1);
  n := 1;

  for i := 0 to Modulos.Count - 1 do
    n *= Modulos[i];
  Result := 0;
  SumMs := 0;
  for i := 0 to Modulos.Count - 1 do
  begin
    //Result += (m div Modulos[i]) * Remainders[i];
    //SumMs += m div Modulos[i];
  end;

  Assert(Result mod SumMs = 0);
  Result := Result div SumMs;

end;

function ModularMultiplicativeInverse(x, m: Int64): Int64;
var
  a, b: Int64;

  procedure RecCompute(x, m: Int64; var a, b: Int64);
  var
    a1, b1: Int64;
  begin
    if x = 1 then
    begin
      //ax + bm = 1
      b := 1;
      a := 1 - m;
    end
    else
    begin
      RecCompute(m mod x, x, a1, b1);
      // a1 m' + b1 x = 1
      // (b1 - a1k) x + a1 (kx + m') = 1
      a := b1 - a1 * (m div x);
      b := a1;
    end;
  end;

begin
  if x < m then
    RecCompute(x, m, a, b)
  else
    RecCompute(m, x, b, a);
  Assert(a * x + b * m = 1);
  Result := a;
  if Result < 0 then
    Result := m - ((m - Result) mod m);

end;

function ModularMultiplicativeInverse4Prime(x, p: Int64): Int64;
begin
  Result := PowMod(x, p - 2, p);
  if (x * Result) mod p <> 1 then
  begin
    WriteLn('x: ', x, ' x^-1: ', Result, ' x*Result: ', (x * Result) mod p);
    Halt(1);
  end;
end;


{ TIntList }

procedure TIntList.Print;
var
  i: Integer;

begin
  if Self.Count <> 0 then
    Write(Self[0]);
  for i := 1 to Self.Count - 1 do
    Write(' ', Self[i]);
  WriteLn;

end;

function TIntList.Sum: Int64;
var
  i: Integer;

begin
  Result := 0;

  for i := 0 to Count - 1 do
    Result += Self[i];

end;

{ TFactorization }

procedure TFactorization.WriteLn;
var
  i: Integer;

begin
  for i := 0 to Self.Count - 1 do
    System.WriteLn(Self[i].Base, '^', Self[i].Power);

end;

{ TFactorizatonPair }

constructor TFactorizationPair.Create;
begin
  inherited Create;

  FBase := 0;
  FPower := 0;
end;

constructor TFactorizationPair.Create(b, p: Integer);
begin
  inherited Create;

  FBase := b;
  FPower := p;
end;

end.

