unit NumberTheoryUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TIntList = specialize TFPGList<Integer>;

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
function IsPrime(n: Integer; const Primes: TIntList): Boolean;
function Factorize(n: Integer; const Primes: TIntList): TFactorization;
function Factorize(n: Integer): TFactorization;
function SumOfDivisors(const Factorization: TFactorization): Int64;
function ToBinary(n: Int64): AnsiString;
{
  Returns sqrt(n) iff n is a perfect square
  Returns -1 o.w.
}
function IsPerfectSquare(n: Int64): Int64;

//  function FindPrimes(Max: Integer);

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

function IsPrime(n: Integer; const Primes: TIntList): Boolean;
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
      if n < Primes[i] * Primes[i] then
        break;
    end;
    Result := True;
  end;

end;

function Factorize(n: Integer; const Primes: TIntList): TFactorization;
var
  i, b, p: Integer;
  Factor: TFactorizationPair;

begin
  Result := TFactorization.Create;
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

    if n < b then
    begin
      if n <> 1 then
        Result.Add(TFactorizationPair.Create(n, 1));
      break;
    end;
  end;

end;

function Factorize(n: Integer): TFactorization;
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

