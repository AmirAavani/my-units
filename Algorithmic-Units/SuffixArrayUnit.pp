{$mode objfpc}
unit SuffixArrayUnit;
interface
uses
  fgl;

type
  TIntList = specialize TFPGList<Integer>;

function GenerateSuffixArray(const S: AnsiString): TIntList;

implementation
type
  TTriple = record
    FirstHalfRank, SecondHalfRank: Integer;
    Index: Integer;
    class operator Set (const a: TTriple): Boolean;
    class operator < (const a, b: TTriple): Boolean;
  end;
  TTripleList = specialize TFPGList<TTriple>;

class operator TTriple.=(const a, b: TTriple): Boolean;
begin
  Result := not (a < b) and not (b < a);
end;

class operator TTriple.<(const a, b: TTriple): Boolean;
begin
  if a.FirstHalfRank < b.FirstHalfRank then
    Exit(True)
  else if b.FirstHalfRank < a.FirstHalfRank then
    Exit(False);
  if a.SecondHalfRank < b.SecondHalfRank then
    Exit(True)
  else if b.SecondHalfRank< a.SecondHalfRank then
    Exit(False);
  if a.Index < b.Index then
    Exit(True)
  else if b.Index < a.Index then
    Exit(False);
  else 
    Exit(False);
end;


procedure SetTriple(f, s, i: Integer; var Triple: TTriple);
begin
  Triple.FirstHalfRank := f;
  Triple.SecondHalfRank := s;
  Triple.Index := i;
end;

function CompareTriples(const T1, T2: TTriple): Integer;
begin
end;

function GenerateSuffixArray(const S: AnsiString): TIntList;
var
  TripleList: TTripleList;
  CurrentSortIndices, NextSortIndices: TIntList;
  CurrentWidth: Integer;
  Triple: TTriple;
  i: Integer;
  N: Integer;

begin
  N := Length(S);
  Result := TIntList.Create;
  if N = 0 then
    Exit(Result);
  if N = 1 then
  begin
    Result.Add(0);
    Exit(Result);
  end;
  
  CurrentSortIndices := TIntList.Create;
  for i := 1 to N do
    CurrentSortIndices.Add(Ord(S[i]));
  TripleList := TTripleList.Create;
  TripleList.Count := N;
  CurrentWidth := 1;
  while CurrentWidth < N do
  begin

    for i := 0 to N - 1 do
TripleList.Items[i]^.FirstHalfRank := CurrentSortIndices[i];
      //SetTriple(CurrentSortIndices[i], CurrentSortIndices[i + CurrentWidth], i, TripleList[i]);
    TripleList.Sort(@CompareTriples);

    CurrentWidth := CurrentWidth * 2;
  end;

  TripleList.Free;

end;

end.
