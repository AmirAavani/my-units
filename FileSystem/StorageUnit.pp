unit StorageUnit;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Generics.Collections;

type

  { TFiles }

  TFiles = class(specialize TList<AnsiString>)
  public
    {
      Pattern can be in form of:
      1) blah@N  -> blah-00001-of-N,blah-00002-of-N, ..., blah-N-of-N
      2) blah@N[s:e] -> blah-s-of-N ... blah-e-1-of-N
    }
    constructor Create(const Pattern: AnsiString);

    {
      Shard partition the members in the object into ShardCount segements, all
      segments, except the last one will have the same size.
      It updates itself to ShardIndex-th such a segments, and return self.
    }
    function Shard(ShardIndex, ShardCount: Integer): TFiles;
  end;

implementation

constructor TFiles.Create(const Pattern: AnsiString);
var
  Ch: Char;
  i: Integer;
  Num: Integer;
  P10: Integer;
  Prefix: AnsiString;
  StartIndex, EndIndex: Integer;

begin
  inherited Create;

  P10 := 1;
  Num := 0;
  i := Length(Pattern);
  StartIndex := -1;
  EndIndex := MaxInt;
  if Pattern[i] = ']' then
  begin
    Dec(i);
    StartIndex := 0; EndIndex:= 0;
    while Pattern[i] <> ':' do
    begin
      Ch := Pattern[i];
      EndIndex += P10 * (Ord(Pattern[i]) - 48);
      P10 *= 10;

      Dec(i);
    end;
    P10 := 1;
    Dec(i);
    while Pattern[i] <> '[' do
    begin
      StartIndex += P10 * (Ord(Pattern[i]) - 48);
      P10 *= 10;

      Dec(i);
    end;
    Dec(i);
  end;

  P10 := 1;
  for i := i downto 1 do
  begin
    Ch := Pattern[i];
    if Ch = '@' then
    begin
      Prefix := Copy(Pattern, 1, i - 1);
      Break;

    end;

    if not (ch in ['0'..'9']) then
    begin
      Break;

    end;
    Num += P10 * (Ord(Ch) - 48);
    P10 *= 10;
  end;

  if StartIndex < 1 then
    StartIndex := 1;
  if Num < EndIndex then
    EndIndex := Num;

  for i := StartIndex to EndIndex do
    Self.Add(Format('%s-%0.5d-of-%0.5d', [Prefix, i, Num]));
end;

function TFiles.Shard(ShardIndex, ShardCount: Integer
  ): TFiles;
var
  i, j: Integer;
  Num: Integer;

begin
  Dec(ShardIndex);
  Num := (Self.Count div ShardCount);
  if ShardIndex = ShardCount - 1 then
    Num += (Self.Count mod ShardCount);
  j := ShardIndex * (Self.Count div ShardCount);
  for i := 0 to Num - 1 do
  begin
    Self[i] := Self[j];
    Inc(j);

  end;
  Self.Count := Num;

  Result := Self;
end;


end.

