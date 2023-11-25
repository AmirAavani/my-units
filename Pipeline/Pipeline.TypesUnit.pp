unit Pipeline.TypesUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StreamUnit, ProtoHelperUnit, Generics.Collections;

type
  TObjectList = specialize TList<TObject>;
  TAnsiStringList = specialize TList<AnsiString>;

  { TPipelineKV }

  generic TPipelineKV<Value: TBaseMessage> = class(specialize TList<specialize TPair<AnsiString, Value>>)
  protected type
    TKeyValuePair = specialize TPair<AnsiString, Value>;
    TPipelineListKeyValue = specialize TPipelineKV<Value>;

  protected

    procedure SaveAnElement(const AnElement: TKeyValuePair; OutputStream: TMyBinStream); virtual;
    function LoadAnElement(InputStream: TMyBinStream): TKeyValuePair; virtual;

    function SaveToStream(OutputStream: TMyBinStream): Boolean; virtual;
    function LoadFromStream(InputStream: TMyBinStream): Boolean; virtual;

  public
    function SaveToFile(OutputFileName: AnsiString): Boolean; virtual;
    constructor LoadFromFile(InputFileName: AnsiString); virtual;
    constructor Create; virtual;

  end;

implementation

{ TPipelineKV }

procedure TPipelineKV.SaveAnElement(const AnElement: TKeyValuePair;
  OutputStream: TMyBinStream);
var
  Data: TBytesStream;

begin
  Data := TBytesStream.Create;
  AnElement.Value.SaveToStream(Data);

  OutputStream.WriteStr(AnElement.Key);
  OutputStream.TargetStream.WriteBuffer(Data.Bytes, Data.Size);
end;

function TPipelineKV.LoadAnElement(InputStream: TMyBinStream): TKeyValuePair;
begin

end;

function TPipelineKV.SaveToStream(OutputStream: TMyBinStream): Boolean;
var
  Element: TKeyValuePair;

begin
  OutputStream.WriteInt(Self.Count);

  for Element in Self do
    SaveAnElement(Element, OutputStream);

  Result := True;

end;

function TPipelineKV.LoadFromStream(InputStream: TMyBinStream): Boolean;
var
  i: Integer;
  ElementCount: Integer;
  Element: TKeyValuePair;

begin
  ElementCount := InputStream.ReadInt32;
  Self.Count := ElementCount;

  for i := 0 to ElementCount - 1 do
  begin
    Element := LoadAnElement(InputStream);
    Self[i] := Element;

  end;
  Result := True;

end;

function TPipelineKV.SaveToFile(OutputFileName: AnsiString): Boolean;
var
  OutputStream: TMyBinStream;

begin
  inherited Create;

  OutputStream := TMyBinStream.Create(TFileStream.Create(OutputFileName, fmCreate), True);
  Result := Self.SaveToStream(OutputStream);
  OutputStream.Free;

end;

constructor TPipelineKV.LoadFromFile(InputFileName: AnsiString);
var
  InputStream: TMyBinStream;

begin
  inherited Create;

  InputStream := TMyBinStream.Create(TFileStream.Create(InputFileName, fmOpenRead), True);
  Self.LoadFromStream(InputStream);
  InputStream.Free;

end;

constructor TPipelineKV.Create;
begin
  inherited Create;

end;

end.

