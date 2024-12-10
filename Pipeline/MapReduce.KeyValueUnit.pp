unit MapReduce.KeyValueUnit;

{$mode ObjFPC}{$H+}
{$modeswitch ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, ProtoHelperUnit, Generics.Collections, StreamUnit;

type

  { TKeyValue }

  TKeyValue = record
  public
    Key: AnsiString;
    Value: TBaseMessage;

    procedure MustSaveToStream(Stream: TStream);
    procedure MustLoadFromStream(Stream: TStream);

    procedure Init(constref k: AnsiString; v: TBaseMessage);
  end;

implementation

uses
  ALoggerUnit;
{ TKeyValue }

procedure TKeyValue.MustSaveToStream(Stream: TStream);
begin
  Stream.WriteAnsiString(Key);
  Value.SaveToStream(Stream);

end;

procedure TKeyValue.MustLoadFromStream(Stream: TStream);
begin
  ALoggerUnit.FmtFatalLnIFFalse(False, 'NIY!', []);
  Self.Key := Stream.ReadAnsiString;
  // Self.Value := TValue.Create;
  Self.Value.SaveToStream(Stream);

end;

procedure TKeyValue.Init(constref k: AnsiString; v: TBaseMessage);
begin
  Key := k;
  Value := v;

end;

end.
