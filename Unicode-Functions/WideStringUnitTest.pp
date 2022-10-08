program WideStringUnitTest;

uses
  WideStringUnit;

procedure Test1;
const
  // Text: AnsiString = 'این ماده در شیمی آلی یک ماده پایه است که در تولید میلیونها ماده آلی دیگر نقش اساسی را بازی میکند';
  Text: AnsiString = 'نقش';
var
  wText: WideString;
  Ch: WideChar;

begin
  wText := ReadWideStringFromString(Text);

  for Ch in wText do
    WriteLn(Ch);

end;

begin
  Test1;

end.
