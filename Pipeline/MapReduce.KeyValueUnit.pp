unit MapReduce.KeyValueUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TByteArray = array of Byte;

  { TKeyValue }

  TKeyValue = record
    Key: AnsiString;
    Value: TByteArray;

  end;

function CopyValue(constref InputKV: TKeyValue; constref NewKey: AnsiString): TKeyValue;

implementation

function CopyValue(constref InputKV: TKeyValue;
  constref NewKey: AnsiString): TKeyValue;
begin
  Result.Key := NewKey;
  SetLength(Result.Value, Length(InputKV.Value));
  Move(InputKV.Value, Result.Value, Length(InputKV.Value));

end;

end.
