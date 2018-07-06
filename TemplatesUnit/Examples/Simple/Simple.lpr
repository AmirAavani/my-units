program Simple;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, TemplateEngineUnit
  { you can add units after this };


procedure Test1;
const
  TemplateString: AnsiString = ('as{{@A}} bc{{@BC}} as@a{{@A}}');
var
  Mapper: TName2ValueMapper;
  Template: TTemplateEngine;
  Response: AnsiString;

begin
  Template := TTemplateEngine.CreateFromText(TemplateString);

  Mapper := TName2ValueMapper.Create;

  Mapper.AddNameValue('A', 'A');
  Mapper.AddNameValue('BC', 'BC');

  Response := Template.Map(Mapper);
  if Response <> 'asA bcBC as@aA' then
    WriteLn(Format('Expected: "%s" Recieved: "%s"', ['asA bcBC as@aA', Response]));

  Mapper.AddNameValue('A', 'A12');
  Response := Template.Map(Mapper);
  if Response <> 'asA12 bcBC as@aA12' then
    WriteLn(Format('Expected: "%s" Recieved: "%s"', ['asA12 bcBC as@aA12', Response]));

  Mapper.AddNameValue('BC', 'B1232C');
  Response := Template.Map(Mapper);
  if Response <> 'asA12 bcB1232C as@aA12' then
    WriteLn(Format('Expected: "%s" Recieved: "%s"', ['asA12 bcB1232C as@aA12', Response]));

  Mapper.AddNameValue('C', 'A12');
  if Response <> 'asA12 bcB1232C as@aA12' then
    WriteLn(Format('Expected: "%s" Recieved: "%s"', ['asA12 bcBC as@aA12', Response]));

  Mapper.AddNameValue('A', 'B1232D');
  Response := Template.Map(Mapper);
  if Response <> 'asB1232D bcB1232C as@aB1232D' then
    WriteLn(Format('Expected: "%s" Recieved: "%s"', ['asB1232D bcB1232C as@aB1232D', Response]));

  Mapper.Free;
  Template.Free;

end;

begin
  Test1;

end.

