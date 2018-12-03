unit SessionManagerUnit;

{$mode objfpc}{$H+}

interface

uses
  SessionDataUnit, Classes, SysUtils, fgl, KeyValueStorageUnit, ProtoHelperUnit;

type
  TSessionID = AnsiString;

  { TBaseSessionManager }

  TBaseSessionManager = class(TObject)
  private
    FSessionIDLen: Integer;
    FMaxSessionValidity: Integer;
  protected
    Mutex: TRTLCriticalSection;
    procedure Load; virtual; abstract;
    procedure Save; virtual; abstract;

    function SessionExists(const SessionID: TSessionID): Boolean; virtual; abstract;
    procedure AddNewSession(Session: TSessionInfo); virtual; abstract;

  public
    constructor Create(SessionIDLen: Integer = 20; MaxSessionValidity: Integer = 3600);
    destructor Destroy; override;

    function CreateNewSessionID: TSessionID; virtual;

    function GetValueByName(const SessionID: TSessionID; const aName: AnsiString): AnsiString; virtual; abstract;
    function SetValue(const SessionID, Name, Value: AnsiString): Boolean; virtual; abstract;
  end;

  { TSessionManager }

  TSessionManager = class(TBaseSessionManager)
  public
  private type
    TMemory = specialize TFPGMap<AnsiString, TSessionInfo>;
  private
    Memory: TMemory;
    SessionFilename: AnsiString;

    function GetValueIndexByName(const SessionInfo: TSessionInfo; const aName: AnsiString): Integer;
  protected
    procedure Load; override;
    procedure Save; override;

    function SessionExists(const SessionID: TSessionID): Boolean; override;
    procedure AddNewSession(Session: TSessionInfo); override;
  public
    constructor Create(Filename: AnsiString; SessionIDLen: Integer = 20; MaxSessionValidity: Integer = 3600);
    destructor Destroy; override;

    function GetValueByName(const SessionID: TSessionID; const aName: AnsiString): AnsiString; override;
    function SetValue(const SessionID, aName, aValue: AnsiString): Boolean; override;
  end;


implementation
uses
  StreamUnit;

{ TBaseSessionManager }

constructor TBaseSessionManager.Create(SessionIDLen: Integer;
  MaxSessionValidity: Integer);
begin
  inherited Create;

  FSessionIDLen := SessionIDLen;
  FMaxSessionValidity := MaxSessionValidity;
end;

destructor TBaseSessionManager.Destroy;
begin
  Self.Save;

  inherited Destroy;
end;

function TBaseSessionManager.CreateNewSessionID: TSessionID;
  function GetCode(Index: Integer): Char;
  begin
    if Index < 26 then
      Result := Chr(65 + Index)
    else if Index < 52 then
      Result := Chr(71 + Index)
    else
      Result := Chr(Index - 4);
  end;

var
  i: Integer;
  NewSessionInfo: TSessionInfo;

begin
  SetLength(Result, FSessionIDLen);
  while True do
  begin

    for i := 1 to FSessionIDLen do
      Result[i] := GetCode(Random(62));

    EnterCriticalSection(Mutex);
    if not SessionExists(Result) then
    begin
      NewSessionInfo := TSessionInfo.Create;
      NewSessionInfo.SessionID := Result;
      NewSessionInfo.CreationTimestamp := DateTimeToTimeStamp(Now).Time;
      NewSessionInfo.LastActivityTimestamp := NewSessionInfo.CreationTimestamp;

      AddNewSession(NewSessionInfo);

      LeaveCriticalSection(Mutex);

      Break;
    end;
    LeaveCriticalSection(Mutex);

  end;

end;

function TSessionManager.GetValueByName(const SessionID: TSessionID;
  const aName: AnsiString): AnsiString;
var
  SessionInfo: TSessionInfo;
  Index: Integer;

begin
  Result := '';

  EnterCriticalSection(Mutex);

  if not Memory.Find(SessionID, Index) then
  begin
    LeaveCriticalSection(Mutex);
    Exit;

  end;

  SessionInfo := Memory[SessionID];
  SessionInfo.LastActivityTimestamp := DateTimeToTimeStamp(Now).Time;

  Index := Self.GetValueIndexByName(SessionInfo, aName);
  if Index = -1 then
  begin
    LeaveCriticalSection(Mutex);
    Exit;
  end;

  Result := SessionInfo.ConstAllNameValues[Index].Value;

  LeaveCriticalSection(Mutex);
end;

function TSessionManager.GetValueIndexByName(const SessionInfo: TSessionInfo;
  const aName: AnsiString): Integer;
var
  Top, Bot, Mid: Integer;

begin
  Bot := 0;
  Top := SessionInfo.ConstAllNameValues.Count - 1;

  Result  := -1;
  while Bot <= Top do
  begin
    Mid := (Bot + Top) div 2;

    if SessionInfo.ConstAllNameValues[Mid].Name < aName then
      Bot := Mid + 1
    else if aName < SessionInfo.ConstAllNameValues[Mid].Name then
      Top := Mid - 1
    else
    begin
      Result := Mid;
      Break;
    end;
  end;

end;

procedure TSessionManager.Load;
var
  InputStream: TMyBinStream;
  i: Integer;
  Tmp: AnsiString;

begin
  InputStream := TMyBinStream.Create(TFileStream.Create(SessionFilename, fmOpenRead), True);
  if InputStream.TargetStream.Size = 0 then
  begin
    InputStream.Free;
    Exit;
  end;

  Memory.Count := InputStream.ReadInt;

  for i := 0 to Memory.Count - 1 do
  begin
    Tmp := InputStream.ReadStr;
    Memory.Keys[i] := Tmp;

    Tmp := InputStream.ReadStr;
    Memory.Data[i] := TSessionInfo.Create;
    Memory.Data[i].LoadFromString(Tmp);
  end;

  InputStream.Free;

end;

procedure TSessionManager.Save;
var
  i: Integer;
  OutputStream: TMyBinStream;
  Tmp: AnsiString;

begin
  OutputStream := TMyBinStream.Create(TFileStream.Create(SessionFilename, fmCreate));

  OutputStream.WriteInt(Memory.Count);
  for i := 0 to Memory.Count - 1 do
  begin
    OutputStream.WriteStr(Memory.Keys[i]);
    Memory.Data[i].SaveToString(Tmp);
    OutputStream.WriteStr(Tmp);
  end;

  OutputStream.Free;
end;

function TSessionManager.SessionExists(const SessionID: TSessionID): Boolean;
begin
  Result := 0 <= Memory.IndexOf(SessionID)
end;

procedure TSessionManager.AddNewSession(Session: TSessionInfo);
begin
  Memory.Add(session.SessionID, Session);

end;

constructor TSessionManager.Create(Filename: AnsiString; SessionIDLen: Integer;
  MaxSessionValidity: Integer);
begin
  inherited Create;

  FSessionIDLen := SessionIDLen;
  Memory := TMemory.Create;
  Memory.Sorted := True;

  SessionFilename := Filename;
  Self.Load;
end;

destructor TSessionManager.Destroy;
begin
  inherited Destroy;
end;

function TSessionManager.SetValue(const SessionID, aName, aValue: AnsiString
  ): Boolean;
var
  SessionInfo: TSessionInfo;
  Index: Integer;
  i: Integer;
  Bot, Mid, Top: Integer;

begin
  EnterCriticalSection(Mutex);

  if not Memory.Find(SessionID, Index) then
  begin
    LeaveCriticalSection(Mutex);
    Exit(False);
  end;

  SessionInfo := Memory[SessionID];
  SessionInfo.LastActivityTimestamp := DateTimeToTimeStamp(Now).Time;


  Index := -1;
  Bot := 0;
  Top := SessionInfo.ConstAllNameValues.Count - 1;

  while Bot <= Top do
  begin
    Mid := (Bot + Top) div 2;

    if SessionInfo.ConstAllNameValues[Mid].Name < aName then
    begin
      Bot := Mid + 1;
      Index := Mid;
    end
    else if aName < SessionInfo.ConstAllNameValues[Mid].Name then
      Top := Mid - 1
    else
    begin
      Index := Mid;
      Break;
    end;
  end;

  if (Index <> -1) and (SessionInfo.ConstAllNameValues[Index].Name = aName) then
  begin
    SessionInfo.ConstAllNameValues[Index].Value := aValue;

    LeaveCriticalSection(Mutex);
    Exit(True);
  end;

  Result := True;

  SessionInfo.AllNameValues.Add(nil);

  for i := SessionInfo.AllNameValues.Count - 2 downto Index + 1 do
    SessionInfo.AllNameValues[i + 1] := SessionInfo.AllNameValues[i];
  SessionInfo.AllNameValues[Index + 1] := TNameValue.Create(aName, aValue);;

  for i := SessionInfo.AllNameValues.Count - 2 downto 0 do
    if SessionInfo.AllNameValues[i + 1].Name < SessionInfo.AllNameValues[i].Name then
      Halt(0);

  LeaveCriticalSection(Mutex);

end;

end.

