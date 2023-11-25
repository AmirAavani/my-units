program WideStringUnitTest;

uses
  cthreads, WideStringUnit;

procedure TestRead;
const
  Text: AnsiString = 'این ماده در شیمی آلی یک ماده پایه است که در تولید میلیونها ماده آلی دیگر نقش اساسی را بازی میکند';
  // Text: AnsiString = 'نقش';
var
  wText: WideString;
  Ch: WideChar;

begin
  wText := ReadWideStringFromString(Text);
  WriteLn(wText);

  for Ch in wText do
    WriteLn(Ch);

end;

procedure TestSplit;
var
  w1, w2, w3: WideString;
  List: TWideStringList;

begin
  w1 := WideStringUnit.ReadWideStringFromString('1234');
  w2 := WideStringUnit.ReadWideStringFromString('امیر');
  w3 := WideStringUnit.ReadWideStringFromString('abcd');

  List := WideStringUnit.WideStrSplit(w1, '2');
  WriteLn(List.Count, ':', List[0], ':', List[1]);
  List.Free;

  List := WideStringUnit.WideStrSplit(w2, WideStringUnit.ReadWideStringFromString('ی')[1]);
  WriteLn(List.Count, ':', List[0], ':', List[1]);
  List.Free;

  List := WideStringUnit.WideStrSplit(w3, 'd', True);
  WriteLn(List.Count, ':', List[0], ':', List[1]);
  List.Free;

end;

procedure TestJoin;
var
  w1, w2, w3: WideString;
  List: TWideStringList;
  S: WideString;

begin
  w1 := WideStringUnit.ReadWideStringFromString('1234');
  w2 := WideStringUnit.ReadWideStringFromString('4567');
  w3 := WideStringUnit.ReadWideStringFromString('abcd');

  List := TWideStringList.Create;
  List.Add(w1); List.Add(w2); List.Add(w3);

  S := List.JoinStrings(':');
  WriteLn('S: ', S);

  List.Clear;
  S := List.JoinStrings(':');
  WriteLn('S: ', S);

  List.Add(w1);
  S := List.JoinStrings(':');
  WriteLn('S: ', S);

  List.Add(w2);
  S := List.JoinStrings(':');
  WriteLn('S: ', S);

  List.Add(w3);
  S := List.JoinStrings(':');
  WriteLn('S: ', S);

  List.Free;


end;

begin
  TestRead;
  TestSplit;
  TestJoin;

end.
