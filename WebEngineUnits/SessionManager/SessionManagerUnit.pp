unit SessionManagerUnit;

{$mode objfpc}{$H+}

interface

uses
  SessionDataUnit, Classes, SysUtils, fgl, KeyValueStorageUnit, ValueUnit;

type

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
    procedure AddNewSession(Session: TSessionData); virtual; abstract;

  public
    constructor Create(SessionIDLen: Integer = 20; MaxSessionValidity: Integer = 3600);
    destructor Destroy; override;

    function CreateNewSessionID: TSessionID; virtual;

    function GetValueByName(const SessionID: TSessionID; const aName: AnsiString): TSessionValue; virtual; abstract;
    function SetValue(const SessionID, Name: AnsiString; Value: TSessionValue): Boolean; virtual; abstract;

    class function GetSessionManager: TBaseSessionManager;
  end;

  { TSessionManager }

  TSessionManager = class(TBaseSessionManager)
  public
  private type
    TMemory = specialize TFPGMap<AnsiString, TSessionData>;
  private
    Memory: TMemory;

  protected
    function SessionExists(const SessionID: TSessionID): Boolean; override;
    procedure AddNewSession(Session: TSessionData); override;
  public
    constructor Create(SessionIDLen: Integer = 20; MaxSessionValidity: Integer = 3600);
    destructor Destroy; override;

    function GetValueByName(const SessionID: TSessionID; const aName: AnsiString): TSessionValue; override;
    function SetValue(const SessionID, aName: AnsiString; aValue: TSessionValue): Boolean; override;
  end;


implementation
uses
  StreamUnit, ALoggerUnit;

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
  NewSessionData: TSessionData;

begin
  SetLength(Result, FSessionIDLen);
  while True do
  begin

    for i := 1 to FSessionIDLen do
      Result[i] := GetCode(Random(62));

    EnterCriticalSection(Mutex);
    if not SessionExists(Result) then
    begin
      NewSessionData := TSessionData.Create(Result, TNameValueMap.Create);

      AddNewSession(NewSessionData);

      LeaveCriticalSection(Mutex);

      Break;
    end;
    LeaveCriticalSection(Mutex);

  end;

end;

class function TBaseSessionManager.GetSessionManager: TBaseSessionManager;
begin
  Result := TSessionManager.Create;
end;

function TSessionManager.GetValueByName(const SessionID: TSessionID;
  const aName: AnsiString): TSessionValue;
var
  SessionInfo: TSessionData;
  Index: Integer;

begin
  Result := nil;

  EnterCriticalSection(Mutex);

  if not Memory.Find(SessionID, Index) then
  begin
    LeaveCriticalSection(Mutex);
    Exit;

  end;

  SessionInfo := Memory[SessionID];
  SessionInfo.UpdateLastActivity;

  if SessionInfo.NameValueMap.Find(aName, Index) then
  begin
    LeaveCriticalSection(Mutex);
    Exit;
  end;

  Result := SessionInfo.NameValueMap[aName];

  LeaveCriticalSection(Mutex);
end;

function TSessionManager.SessionExists(const SessionID: TSessionID): Boolean;
begin
  Result := 0 <= Memory.IndexOf(SessionID)
end;

procedure TSessionManager.AddNewSession(Session: TSessionData);
begin
  Memory.Add(session.SessionID, Session);

end;

constructor TSessionManager.Create(SessionIDLen: Integer;
  MaxSessionValidity: Integer);
begin
  inherited Create;

  FSessionIDLen := SessionIDLen;
  Memory := TMemory.Create;
  Memory.Sorted := True;

end;

destructor TSessionManager.Destroy;
begin
  inherited Destroy;
end;

function TSessionManager.SetValue(const SessionID, aName: AnsiString;
  aValue: TSessionValue): Boolean;
var
  SessionInfo: TSessionData;
  Index: Integer;

begin
  EnterCriticalSection(Mutex);

  if not Memory.Find(SessionID, Index) then
  begin
    LeaveCriticalSection(Mutex);
    Exit(False);
  end;

  SessionInfo := Memory[SessionID];
  SessionInfo.UpdateLastActivity;;

  Index := -1;
  if SessionInfo.NameValueMap.Find(aName, Index) then
  begin
    SessionInfo.NameValueMap.Data[Index].Free;
    SessionInfo.NameValueMap.Data[Index] := aValue;

    LeaveCriticalSection(Mutex);
    Exit(True);
  end;

  Result := False;

  SessionInfo.NameValueMap[aName] := aValue;

  LeaveCriticalSection(Mutex);

end;

end.

