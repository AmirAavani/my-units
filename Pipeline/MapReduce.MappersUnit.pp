unit MapReduce.MappersUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ProtoHelperUnit, MapReduce.NodeUnit;

type

  { TSplitByDelimiterMapper }

  TSplitByDelimiterMapper = class(TMapper)
  private
    Delimiter: Char;

  public
    constructor Create(Delim: Char);

    function Process(constref InputKey: AnsiString; InputValue: TBaseMessage;
      Sender: TNode): Boolean; override;


  end;

implementation
uses
  SimpleTypesUnit;

{ TSplitByDelimiterMapper }

constructor TSplitByDelimiterMapper.Create(Delim: Char);
begin
  inherited Create;

  Delimiter := Delim;
end;

function TSplitByDelimiterMapper.Process(constref InputKey: AnsiString;
  InputValue: TBaseMessage; Sender: TNode): Boolean;
var
  Text: AnsiString;
  Current: AnsiString;
  Last: PChar;
  Ch: PChar;
  i: Integer;

begin
  if InputValue <> nil then
  begin
    WriteLn('Unexpected Entry!');
    Halt(2);

  end;

  Text := InputKey;
  WriteLn(Format('Mapper %d -> %s', [ThreadID, Text]));
  Current := '';
  Ch := @Text[1];
  Dec(Ch);
  Last := Ch;

  for i := 1 to Length(Text) do
  begin
    Inc(Ch);
    if Ch^ = Delimiter then
    begin
      if Ch <> Last then
      begin
        SetLength(Current, Ch - Last - 1);
        Move((Last + 1)^, Current[1], Ch - Last - 1);
        WriteLn(Format('Mapper %d -> %s', [ThreadID, Current]));
        Sender.Send(Current, nil);
      end;
      Last := Ch;
      Continue;

    end;

    Current += Ch;

  end;

  if Ch <> Last then
  begin
    SetLength(Current, Ch - Last);
    Move((Last + 1)^, Current[1], Ch - Last);
    Sender.Send(Current, nil);
  end;

  Result := True;
end;

end.

