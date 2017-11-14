{$mode objfpc}
program SuffixArray;
uses
  SuffixArrayUnit;
{$modeswitch advanced_records}

var
  S: AnsiString;
  SArray: TIntList;
  i: Integer;

begin
  ReadLn(S);

  SArray := GenerateSuffixArray(S);

  for i := 0 to SArray.Count - 1 do
    WriteLn(Copy(S, SArray[i], Length(S)));

end.
