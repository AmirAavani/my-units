unit KeyValueStorageUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, StreamUnit;

type
  generic TInMemoryStorage<TValue> = class(TObject)
  private type
    TMemory = specialize TFPGMap<AnsiString, TValue>;

  private
    Mutex: TRTLCriticalSection;
    FMemory: TMemory;
    FFileName: AnsiString;

  private
    function GetValueByKey(const Key: AnsiString): TValue; virtual;

  protected
    property ValueByKey[const Key: AnsiString]: TValue read GetValueByKey;

  public
    constructor Create(InputFilename: AnsiString);
    destructor  Destroy; override;

    // Returns True iff the key does not exists.
    function AddOrUpdate(const key: AnsiString;  const ValueStr: AnsiString): Boolean;
    // Returns True iff the key does not exists.
    function AddOrUpdateValue(const key: AnsiString; const Value: TValue): Boolean;
    // Returns True iff the key exists.
    function DeleteKey(const key: AnsiString): Boolean;
    // Returns nil (and sets PIndex to -1) iff the key does not exists.
    function GetByKey(const Key: AnsiString; PIndex: PInteger = nil): TValue;

    procedure SaveToFile;
  end;


implementation

{ TInMemoryKeyValyeStorage }

constructor TInMemoryStorage.Create(InputFilename: AnsiString);
var
  InputStream: TMyBinStream;
  i: Integer;
  tmp: AnsiString;

begin
  inherited Create;

  FMemory := TMemory.Create;
  FMemory.Sorted := True;

  FFileName := InputFilename;

  InputStream := TMyBinStream.Create(TFileStream.Create(FFilename, fmOpenRead), True);
  if InputStream.TargetStream.Size = 0 then
  begin
    InputStream.Free;
    Exit;
  end;

  FMemory.Count := InputStream.ReadInt;

  for i := 0 to FMemory.Count - 1 do
  begin
    FMemory.Keys[i] := InputStream.ReadStr;

    tmp := InputStream.ReadStr;
    FMemory.Data[i] := TValue.Create;
    FMemory.Data[i].LoadFromString(Tmp);
  end;

  InputStream.Free;
end;

destructor TInMemoryStorage.Destroy;
var
  OutputStream: TMyBinStream;

begin
  if FFileName <> '' then
  begin
    OutputStream := TMyBinStream.Create(TFileStream.Create(FFileName, fmCreate));
    Self.SaveToFile;

    OutputStream.Free;
  end;

  inherited Destroy;
end;

procedure TInMemoryStorage.SaveToFile;
var
  i: Integer;
  OutputStream: TMyBinStream;
  Tmp: AnsiString;

begin
  OutputStream := TMyBinStream.Create(TFileStream.Create(FFileName, fmCreate), True);

  OutputStream.WriteInt(FMemory.Count);
  for i := 0 to FMemory.Count - 1 do
  begin
    OutputStream.WriteStr(FMemory.Keys[i]);
    FMemory.Data[i].SaveToString(Tmp);
    OutputStream.WriteStr(Tmp);
  end;

  OutputStream.Free;
end;

{ TKeyValueStorage }

function TInMemoryStorage.GetValueByKey(const Key: AnsiString): TValue;
begin
  EnterCriticalSection(Mutex);

  Result := FMemory[Key];

  LeaveCriticalSection(Mutex);

end;

function TInMemoryStorage.AddOrUpdate(const key: AnsiString;
  const ValueStr: AnsiString): Boolean;
var
  aValue: TValue;

begin
  aValue := TValue.Create;
  aValue.LoadFromString(ValueStr);

  Result := Self.AddOrUpdateValue(Key, aValue);
end;

function TInMemoryStorage.AddOrUpdateValue(const key: AnsiString; const Value: TValue
  ): Boolean;
var
  Index: Integer;

begin
  Result := True;

  EnterCriticalSection(Mutex);

  Index := FMemory.IndexOf(Key);

  if Index <> -1 then
  begin
    FMemory.Data[Index].Free;
    FMemory.Delete(Index);
    LeaveCriticalSection(Mutex);
    Result := False;
  end;

  FMemory[key] := Value;

  LeaveCriticalSection(Mutex);

end;

function TInMemoryStorage.DeleteKey(const key: AnsiString): Boolean;
var
  Index: Integer;

begin
  Result := False;

  EnterCriticalSection(Mutex);

  Index := FMemory.IndexOf(Key);
  if Index <> -1 then
  begin
    FMemory.Data[Index].Free;
    FMemory.Delete(Index);
    Result := True;
  end;

  LeaveCriticalSection(Mutex);

end;

function TInMemoryStorage.GetByKey(const Key: AnsiString; PIndex: PInteger): TValue;
var
  Index: Integer;

begin
  EnterCriticalSection(Mutex);

  Index := FMemory.IndexOf(Key);
  if PIndex <> nil then
    PIndex^ := Index;
  Result := FMemory.Data[Index];

  LeaveCriticalSection(Mutex);
end;

end.

